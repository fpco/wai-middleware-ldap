# wai-middlware-ldap

## Install

Install external dependencies:

```bash
$ sudo apt-get install -y libldap2-dev libsasl2-dev
```

Then install using stack:

```bash
$ cd wai-middleware-ldap
$ stack install
[...]
Copied executables to /home/user/.local/bin:
- wai-ldap
```

Make sure above path is in your `$PATH`.

## Configure

Create a config file in yaml format. To get more info on the file format and
secret key generation check out
parent
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
    servers:
      - host: "localhost"
        bind_dn: "cn=admin,dc=example,dc=com"
        bind_dn_password: "password"
        base_user_dn: "dc=example,dc=com"
        scope: "subtree"
        filter: "(&(objectClass=user)(uidNumber=*)(unixHomeDirectory=*))"
```

## Execute

```bash
$ wai-ldap --config-file /path/to/ldap-config.yaml
Listening on port 3000
```

## Caveats

In case LDAP server has a SSL sertificate that is not signed by a known CA,
that CA certificate needs to be either:

1. Installed globally on system:

```bash
$ sudo cp ca-custom.crt /usr/local/share/ca-certificates/ca-custom.crt
$ sudo update-ca-certificates
```

2. Or supplied through an environment variable:

```bash
$ env LDAPTLS_CACERT=custom-ca.crt wai-ldap --config=ldap-config.yaml
```

In order to get servers CA certificate:

```bash
$ openssl s_client -showcerts -connect ldap.server.com:636
```
