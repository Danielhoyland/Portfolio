# Innoveria Bachelor

This is the main repository for group 203 in the Spring 2024 semester programming bacheleor.

The application consists of three distinct programs created by us, one chirpstack instance and a database. The input API, output API and the react webapplication.

## Adding a gateway

Adding a gateway is likely the most complex thing a normal user has to do, but it is not that hard.

First you need a gateway, power it up and connect an ethernet cable (etherenet cable is optional)
When it is powered on you need to connect to its web interface. To do this you need to connect to its WI-FI (it should be named something like RAK-####, where # are numbers or letters). When the connection is successfull you need to enter the IP adress (found in the instruction manual) into the search bar of your favorite web browser.
If there is no available ethernet cable you will have to connect via WI-FI.
(insert image of the wifi with arrows)

When the WI-FI successfully connects you can go to the settings, enter the correct variables, shown in the image below
(insert image of the setting where you are supposed to go, with the correct IP adress in, with red circles and arrows)

Finally you can add the gateway using the website interface. You can find the EUI either on the pamphlet in the box, or in the web-interface of the gateway.


## Input API

The input API is the program reciving data from the chirpstack instance. Then the program saves the new packet in the database, and in memory if any of the owners of the packet is logged in on the web application. This packet that is saved in memory can then be retrived later via an exposed endpoint.

## Output API

The output API has only one job, and that is reading data from the database, and exposing this data via endpoints. 

## Chirpstack

We use [Chirpstack](https://www.chirpstack.io), which is an open source LoRaWAN network server. This is hosted locally using docker, and interacted with using their built in API. The point of this is to allow a connection between our application and off-site sensors. 

## React webapp

This is the user interface of the app, created using react, this app should visualize the electricity usage data sendt by the sensors.

## Install instructions

Instructions on how to install this on a virtual machine over ssh:

NEED TO HAVE A TLS CERTIFICATE AND KEY WITHIN FOLDER:
"*user_home_dir*/etc/letsencrypt/live/"domain_name"/fullchain.pem
"*user_home_dir*/etc/letsencrypt/live/"domain_name"/privkey.pem

Before starting remember that certain changes will have to be done in order to use TLS, (such as changing the paths in the docker compose files and dockerfiles for TLS keys), and this guide will not take these things into account.

1. Connect to the machine over ssh.
2. Install docker onto the machine
3. Pull the project repository, or move the files to the virtual machine some other way
4. run the command "sudo docker compose -f docker-compose.yaml up --build -d"
5. Set up the latest Chirpstack docker container, found [here](https://hub.docker.com/r/chirpstack/chirpstack/tags)
    1. After starting the container, set up a channel through ssh to a local computer, command: `ssh -D 8080 -C -N "username"@"ip_adress"`. replace "username" with the username and "ip_adress" with the ip adress.
    2. open firefox, go to setting, open proxy settings, set to manual proxy settings, SOCKS-server: 127.0.0.1, port 8080, socks v5.
    3. going to the search bar, type http://172.20.0.7:8080/ and login. Default password/username is admin
    4. create an API key and save it somewhere secure.
    5. Create a device profile with these parameters (unspecified is default)
        1. region EU868
        2. MAC version: LoraWAN 1.0.2
        3. ADR algorithm: LoRa & LR-FHSS ADR algorithm 
        4. expected uplink: 60 sec
        5. regional parameters: A
        6. supports OTAA: true
        7. payload codec: javascript functions
        8. Codec functions: copy text in *CODEC.md*
    6. Copy the device profile id and save it somewhere
    7. create an application and save the application id
    8. click dashboard and save the tenant id
6. connect all chirpstack docker instances to the docker network created by our docker compose file
7. Create a MariaDB container, connect to it, and execute the SQL file found in our project repository (under backend/commands.sql) call the database "test"
8. Connect MariaDB docker instance to the same network as the previously stated network.
9. Create a folder in the root directory called "envars", and within there two folders called "egress" and "ingress". inside each of these create a folder called ".env".
    1. inside egress/.env add the following entries:
        1. DB_PASS="your_mariadb_pass"
        2. DB_PATH="docker_ip_adress_to_mariadb_instance" (hint use sudo docker network inspect "network name")
        3. WEB_URL=https://"your_domain_name or ip_adress"
        4. ADMIN_WEB_URL=https://"your_domain_name or ip_adress":9094
    2. inside ingress/.env add the following entries
        1. API_KEY=Bearer "the_API key you created"
        2. DB_PASS="your mariadb password here"
        3. DB_PATH="the same path as the other .env file"
        4. CHIRP_URL=http://"internal ip adress of the chirpstack instance":8090 (hint use docker newtwork inspect again)
        5. DEVICE_PROFILE_ID="device profile id"
        6. APPLICATION_ID="application id"
        7. TENANT_ID="tenant id"
        8. WEB_URL=https://"your_domain_name or ip_adress"
        9. ADMIN_WEB_URL=https://"your_domain_name or ip_adress":9094
10. Run command from step 4 again, or manually do `sudo docker restart "container id"`for each of the containers.


