# wai-middleware-ldap

## Install

Install external dependencies:

```bash
$ sudo apt-get install -y libldap2-dev libsasl2-dev
```

Then install using stack:

```bash
$ cd wai-middleware-ldap
$ stack install
...
Copied executables to /home/user/.local/bin:
- wai-ldap
```

Make sure above path is in your `$PATH`.

## Configure

Create a config file in yaml format. Generate a new secret key and place it into
config under `secret_key`:

```
$ echo $(stack exec -- wai-ldap key -b)
x1jPsnL5eEp3OsMYD6gRh...
```

To get more info on the file format check out parent
[wai-middleware-auth](https://www.stackage.org/package/wai-middleware-auth)
package. Here is an example config:

```yaml
app_port: 3000
cookie_age: 3600
secret_key: "ohG..."
file_server:
  root_folder: "/path/to/web/files"
  redirect_to_index: true
  add_trailing_slash: true
providers:
  ldap:
    debug: true # default is 'false', should be ommitted or false in production
    bind_dn: "BIND\\USERNAME"
    bind_password: "foobar"
    base_dn: "dc=ldap,dc=example,dc=com"
    scope: "subtree"
    filter: "(&(objectClass=user)(uidNumber=*)(unixHomeDirectory=*))"
    user_search_key: "mail" # default is "uid".
    urls:
      - "ldaps://ldap01.example.com:636"
      - "ldaps://ldap02.example.com:636"
```


## Execute

```bash
$ wai-ldap --config-file /path/to/ldap-config.yaml
Listening on port 3000
```

## Caveats

If you get an error while trying to login and `debug` is set to `true`:

```
Initial binds:
ldapSimpleBind: LDAPException LdapServerDown(-1): Can't contact LDAP server
```
it might indicate that LDAP server has a SSL certificate that is not signed by a known CA, which is fairly common for LDAP servers. You can double check it using `ldapsearch` (if installed):

```bash
$ ldapsearch -x -v -D "BIND\\USERNAME" -w "foobar" -H ldap01.example.com:636 -b "dc=ldap,dc=example,dc=com" -s sub "uid=username"
```

In that case CA certificate needs to be either:

1. Installed globally on the system:

```bash
$ sudo cp ca-custom.crt /usr/local/share/ca-certificates/ca-custom.crt
$ sudo update-ca-certificates
```

2. Or supplied through an environment variable:

```bash
$ env LDAPTLS_CACERT=custom-ca.crt wai-ldap --config=ldap-config.yaml
```

Run this to retrieve SSL CA certificate from the server:

```bash
$ openssl s_client -showcerts -connect ldap01.example.com:636
...
-----BEGIN CERTIFICATE-----
<certificate>
-----END CERTIFICATE-----
...
```

## Docker

### File server

In order to run file server under docker, place your config into `/path/to/config/wai-ldap.yaml`:

```yaml
app_port: 3000
cookie_age: 3600
secret_key: "ohG..."
file_server:
  root_folder: "/web/files"
  redirect_to_index: true
  add_trailing_slash: true
providers:
  ldap:
  - ...
...
```

Then run `fpco/wai-ldap` docker image.

```bash
$ docker run --rm \
    --volume="/path/to/config:/config" \
    --volume="/path/to/website:/website" \
    --publish=80:3000 \
    fpco/wai-ldap
```

### Reverse Proxy

Config file is same as above, except with `reverse_proxy` instead `file_server`. Note the `HOST_IP` environment variable, it's purpose is described later.

```yaml
app_port: 3000
cookie_age: 3600
secret_key: "ohG..."
reverse_proxy:
  host: "_env:HOST_IP:localhost"
  port: 5601
providers:
  ldap:
  - ...
...
```

If the web server you are trying to protect is itself running inside docker, then the easiest way to combine it with `wai-ldap` is to use `docker-compose`. On the other hand, if it is running on the host OS, then we have to, either use `docker run` with `--net=host`, which is not very secure and thus less desireable, or use default `--net=bridge` and let `wai-ldap` know the ip address of `docker0` interface. This is where `HOST_IP` environment variable comes into play.

```bash
$ ifconfig docker0 | grep 'inet addr:' | cut -d: -f2 | awk '{print $1}'
172.17.0.1
$ docker run --rm \
    --volume="/path/to/config:/config" \
    --publish=80:3000 \
    -e HOST_IP=$(ifconfig docker0 | grep 'inet addr:' | cut -d: -f2 | awk '{print $1}') \
    fpco/wai-ldap
```

### Custom SSL certificate

Look at the [Caveats](#caveats) for info about custom CA and debug info.

In order to supply custom CA certificate to `wai-ldap` inside docker container,
simply mount a folder that contains that certificate as
`/usr/local/share/ca-certificates/`:

```bash
$ docker run --rm \
    --volume="/path/to/config:/config" \
    --volume="/path/to/custom/ca-cert/:/usr/local/share/ca-certificates/" \
    --publish=80:3000 \
    -e HOST_IP=$(ifconfig docker0 | grep 'inet addr:' | cut -d: -f2 | awk '{print $1}') \
    fpco/wai-ldap
```
