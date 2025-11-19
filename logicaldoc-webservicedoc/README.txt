1) Aggiornare il file context-webservicedoc.xml e plugin.xml con alla versione 
2) Compilare la applicazione: mvn package
3) Eseguire il deploy: ant deploy
4) Ottenere la documentazione in formato Swagger invocando gli url seguenti

#Gli url da invocare sono i seguenti:
# Documentazione in formato OpenApi 3.0 json
http://localhost:8080/services/rest/folderSWG/openapi.json

# Documentazione in formato OpenApi 3.0 yaml
http://localhost:8080/services/rest/folderSWG/openapi.yaml


http://localhost:8084/services/rest/folderSWG/openapi.json
http://localhost:8084/services/rest/folderSWG/openapi.yaml

http://localhost:8084/services/rest/bookmarkSWG/api-docs?url=http://localhost:8084/services/rest/bookmarkSWG/openapi.json