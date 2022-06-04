# uk.axvr.redirect

Micro web-forwarding and link shortener service powering axvr.uk
infrastructure.


## Connect your domain

### Domain root

- Create an A record pointing to `213.52.130.150`.
- Create an AAAA record pointing to `2a01:7e00::f03c:93ff:fed5:363f`.

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
docker compose up -d
```
