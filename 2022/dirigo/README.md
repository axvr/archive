# Dirigo

Micro web-forwarding and link shortener service powering axvr.uk
infrastructure.


## Connect your domain name

### Domain root

- Create an A record pointing to `18.132.100.149`.

### Subdomain

Create a CNAME record pointing to `axvr.uk.`.


## Running

If running for the first time, you may need to run these commands:

```
git checkout HEAD -- traefik/acme.json
chmod 600 traefik/acme.json
```

Then switch the CA server used in the `traefik/traefik.yml` file and run:

```
docker compose up -d --build
```
