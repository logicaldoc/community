/**
 *
 * (c) Copyright LogicalDOC SRL 2024
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.logicaldoc.onlyoffice.helpers;

import static com.logicaldoc.onlyoffice.utils.Constants.BUFFER_SIZE;
import static com.logicaldoc.onlyoffice.utils.Constants.FILE_SAVE_TIMEOUT;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.primeframework.jwt.domain.JWT;

import com.google.gson.Gson;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.onlyoffice.controllers.OnlyOfficeIndex;
import com.logicaldoc.util.Context;

public final class TrackManager {
	
    private static final String DOCUMENT_JWT_HEADER = ConfigManager.getProperty("files.docservice.header");

    private TrackManager() { }

    // read request body
    public static JSONObject readBody(final HttpServletRequest request, final PrintWriter writer) throws Exception {
    	
        String bodyString = "";

        try {
            // read request body by streams
            Scanner scanner = new Scanner(request.getInputStream());
            scanner.useDelimiter("\\A");
            bodyString = scanner.hasNext() ? scanner.next() : "";
            scanner.close();
        } catch (Exception ex) {
            writer.write("get request.getInputStream error:" + ex.getMessage());
            throw ex;
        }

        // error when the bodyString object is empty
        if (bodyString.isEmpty()) {
            writer.write("empty request.getInputStream");
            throw new Exception("empty request.getInputStream");
        }

        JSONParser parser = new JSONParser();
        JSONObject body;

        try {
            Object obj = parser.parse(bodyString);  // parse bodyString object
            body = (JSONObject) obj;
        } catch (Exception ex) {
            writer.write("JSONParser.parse error:" + ex.getMessage());
            throw ex;
        }

        // if the secret key to generate token exists
        if (OODocumentManager.tokenEnabled() && OODocumentManager.tokenUseForRequest()) {
            String token = (String) body.get("token");  // get the document token

            if (token == null) {  // if JSON web token is not received
                String header = (String) request.getHeader(DOCUMENT_JWT_HEADER == null || DOCUMENT_JWT_HEADER.isEmpty()
                        ? "Authorization" : DOCUMENT_JWT_HEADER);  // get it from the Authorization header
                if (header != null && !header.isEmpty()) {
                    String bearerPrefix = "Bearer ";

                    // and save it without Authorization prefix
                    token = header.startsWith(bearerPrefix) ? header.substring(bearerPrefix.length()) : header;
                }
            }

            if (token == null || token.isEmpty()) {  // if the token is not received
                writer.write("{\"error\":1,\"message\":\"JWT expected\"}");  // an error occurs
                throw new Exception("{\"error\":1,\"message\":\"JWT expected\"}");
            }

            JWT jwt = OODocumentManager.readToken(token);  // read token
            if (jwt == null) {
                writer.write("{\"error\":1,\"message\":\"JWT validation failed\"}");  // an error occurs
                throw new Exception("{\"error\":1,\"message\":\"JWT validation failed\"}");
            }

            if (jwt.getObject("payload") != null) {  // get the payload object from the request body
                try {
                    @SuppressWarnings("unchecked") LinkedHashMap<String, Object> payload =
                            (LinkedHashMap<String, Object>) jwt.getObject("payload");

                    jwt.claims = payload;
                } catch (Exception ex) {
                    writer.write("{\"error\":1,\"message\":\"Wrong payload\"}");
                    throw ex;
                }
            }

            try {
                Gson gson = new Gson();
                Object obj = parser.parse(gson.toJson(jwt.claims));
                body = (JSONObject) obj;
            } catch (Exception ex) {
                writer.write("JSONParser.parse error:" + ex.getMessage());
                throw ex;
            }
        }

        return body;
    }
    

    // file saving process
    public static void processSave(final JSONObject body,
                                   final String fileName,
                                   final String userAddress, HttpServletRequest request) throws Exception {
    	
    	System.out.println("TrackManager.processForceSave()");
    	
    	//list all the parameters of the request
    	Map<String, String[]> allMap = request.getParameterMap();
    	for (String key : allMap.keySet()) {
    	    String[] strArr = (String[]) allMap.get(key);
    	    for (String val : strArr) {
    	        System.out.println(key + " = " + val);
    	    }
    	} 
    	
        if (body.get("url") == null) {
            throw new Exception("DownloadUrl is null");
        }
        String downloadUri = (String) body.get("url");
        String changesUri = (String) body.get("changesurl");
        String key = (String) body.get("key");
        String newFileName = fileName;

        String curExt = FileUtility.getFileExtension(fileName);  // get current file extension
        String downloadExt = (String) body.get("filetype");  // get the extension of the downloaded file
        
        System.out.println("downloadUri: " +downloadUri);
        System.out.println("changesUri: " +changesUri);
        System.out.println("key: " +key);
        System.out.println("newFileName: " +newFileName);
        
        System.out.println("curExt: " +curExt);
        System.out.println("downloadExt: " +downloadExt);
        

        // convert downloaded file to the file with the current extension if these extensions aren't equal
        if (!curExt.equals(downloadExt)) {
        	System.out.println("convert downloaded file to the file with the current extension");
            try {
                String newFileUri = ServiceConverter
                        .getConvertedData(downloadUri, downloadExt, curExt,
                                ServiceConverter.generateRevisionId(downloadUri),
                                null, false, null).get("fileUrl");  // convert file and get url to a new file
                if (newFileUri.isEmpty()) {

                    // get the correct file name if it already exists
                    newFileName = OODocumentManager
                            .getCorrectName(FileUtility
                                    .getFileNameWithoutExtension(fileName) + "." + downloadExt, userAddress);
                } else {
                    downloadUri = newFileUri;
                }
            } catch (Exception e) {
                newFileName = OODocumentManager.getCorrectName(FileUtility
                        .getFileNameWithoutExtension(fileName) + "." + downloadExt, userAddress);
            }
        }

        byte[] byteArrayFile = getDownloadFile(downloadUri);  // download document file
                        
        String sid = request.getParameter("sid");
        System.out.println("sid: " +sid);
        if (sid == null || sid.isEmpty()) {
        	System.out.println("sid: IS EMPTY OR NULL, treminate execution!");
        	return;
        }
        	
        Session session = SessionManager.get().get(sid);
        User ldUser = session.getUser();
        
        long documentID = Long.parseLong(key.substring(0, key.indexOf("-")));
        Document dldDoc = OnlyOfficeIndex.getDocument(documentID, ldUser);
        
        DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(dldDoc.getFolder().getId());
		transaction.setUser(ldUser);
		transaction.setDocId(documentID);
		transaction.setUserId(ldUser.getId());
		transaction.setNotified(0);
		transaction.setComment("onlyoffice checkin");
		
		com.logicaldoc.core.document.DocumentManager ldDM = (com.logicaldoc.core.document.DocumentManager)Context.get().getBean("documentManager");
	
        try (InputStream is = new ByteArrayInputStream(byteArrayFile)) {
			ldDM.checkin(documentID, is, newFileName, false, dldDoc, transaction);
			
			System.out.println("Operation completed");
		} catch (PersistenceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        
        /*
        String storagePath = DocumentManager.storagePath(newFileName, userAddress);  // get the file path
        File histDir = new File(DocumentManager.historyDir(storagePath));  // get the path to the history direction
        if (!histDir.exists()) {
            histDir.mkdirs();  // if the path doesn't exist, create it
        }

        String versionDir = DocumentManager.versionDir(histDir.getAbsolutePath(), DocumentManager
                .getFileVersion(histDir.getAbsolutePath()));  // get the path to the file version
        File ver = new File(versionDir);
        File lastVersion = new File(DocumentManager.storagePath(fileName, userAddress));
        Path toSave = Paths.get(storagePath);

        if (!ver.exists()) {
            ver.mkdirs();
        }

        // get the path to the previous file version and rename the last file version with it
        lastVersion.renameTo(new File(versionDir + File.separator + "prev." + curExt));

        saveFile(byteArrayFile, toSave); // save document file

        byte[] byteArrayChanges = getDownloadFile(changesUri);
        saveFile(byteArrayChanges, Paths.get(versionDir + File.separator + "diff.zip"));

        String history = (String) body.get("changeshistory");
        if (history == null && body.containsKey("history")) {
            history = ((JSONObject) body.get("history")).toJSONString();
        }
        if (history != null && !history.isEmpty()) {

            // write the history changes to the changes.json file
            FileWriter fw = new FileWriter(new File(versionDir + File.separator + "changes.json"));
            fw.write(history);
            fw.close();
        }

        // write the key value to the key.txt file
        FileWriter fw = new FileWriter(new File(versionDir + File.separator + "key.txt"));
        fw.write(key);
        fw.close();

        // get the path to the forcesaved file version
        String forcesavePath = DocumentManager.forcesavePath(newFileName, userAddress, false);
        if (!forcesavePath.equals("")) {  // if the forcesaved file version exists
            File forceSaveFile = new File(forcesavePath);
            forceSaveFile.delete();  // remove it
        }
        */
    }
    
    // file force saving process
    public static void processForceSave(final JSONObject body,
                                        final String fileNameParam,
                                        final String userAddress, HttpServletRequest request) throws Exception {
    	
    	System.out.println("TrackManager.processForceSave()");
    	
    	//list all the parameters of the request
    	Map<String, String[]> allMap = request.getParameterMap();
    	for (String key : allMap.keySet()) {
    	    String[] strArr = (String[]) allMap.get(key);
    	    for (String val : strArr) {
    	        System.out.println(key + " = " + val);
    	    }
    	} 
    	
        if (body.get("url") == null) {
            throw new Exception("DownloadUrl is null");
        }
        
        String fileName = fileNameParam;
        String downloadUri = (String) body.get("url");
        String key = (String) body.get("key");
        String curExt = FileUtility.getFileExtension(fileName);  // get current file extension
        String downloadExt = (String) body.get("filetype");  // get the extension of the downloaded file
        
        System.out.println("downloadUri: " +downloadUri);
        System.out.println("key: " +key);
        System.out.println("curExt: " +curExt);
        System.out.println("downloadExt: " +downloadExt);

        Boolean newFileName = false;

        // convert downloaded file to the file with the current extension if these extensions aren't equal
        if (!curExt.equals(downloadExt)) {
        	System.out.println("convert downloaded file to the file with the current extension");
            try {
                String newFileUri = ServiceConverter
                        .getConvertedData(downloadUri, downloadExt, curExt,
                                ServiceConverter.generateRevisionId(downloadUri), null,
                                false, null).get("fileUrl");  // convert file and get url to a new file
                if (newFileUri.isEmpty()) {
                    newFileName = true;
                } else {
                    downloadUri = newFileUri;
                }
            } catch (Exception e) {
                newFileName = true;
            }
        }

        byte[] byteArrayFile = getDownloadFile(downloadUri);  // download document file
        String forcesavePath = "";
        boolean isSubmitForm = body.get("forcesavetype").toString().equals("3");  // SubmitForm       

        if (isSubmitForm) {  // if the form is submitted
        	System.out.println("the form is submitted");
            // new file
            if (newFileName) {
                fileName = OODocumentManager.getCorrectName(FileUtility.getFileNameWithoutExtension(fileName)
                        + "-form." + downloadExt, userAddress);  // get the correct file name if it already exists
            } else {
                fileName = OODocumentManager.getCorrectName(FileUtility.getFileNameWithoutExtension(fileName)
                        + "-form." + curExt, userAddress);
            }
            forcesavePath = OODocumentManager.storagePath(fileName, userAddress);
        } else {
            if (newFileName) {
                fileName = OODocumentManager.getCorrectName(FileUtility
                        .getFileNameWithoutExtension(fileName) + downloadExt, userAddress);
            }

            // create forcesave path if it doesn't exist
//            forcesavePath = OODocumentManager.forcesavePath(fileName, userAddress, false);
//            if (forcesavePath == "") {
//                forcesavePath = OODocumentManager.forcesavePath(fileName, userAddress, true);
//            }
        }
        
        System.out.println("newFileName: " +newFileName);
        System.out.println("fileName: " +fileName);
        System.out.println("forcesavePath: " +forcesavePath);

        /*
        saveFile(byteArrayFile, Paths.get(forcesavePath));

        if (isSubmitForm) {
            JSONArray actions = (JSONArray) body.get("actions");
            JSONObject action = (JSONObject) actions.get(0);
            String user = (String) action.get("userid");  // get the user id

            // create meta data for forcesaved file
            DocumentManager.createMeta(fileName, user, "Filling Form", userAddress);

            try {
                String formsDataUrl = body.get("formsdataurl").toString();

                if (formsDataUrl != null && !formsDataUrl.isEmpty()) {
                    String formsName = DocumentManager.getCorrectName(FileUtility
                            .getFileNameWithoutExtension(fileName) + ".txt", userAddress);
                    String formsPath = DocumentManager.storagePath(formsName, userAddress);

                    byte[] byteArrayFormsData = getDownloadFile(formsDataUrl);

                    saveFile(byteArrayFormsData, Paths.get(formsPath));
                } else {
                    throw new Exception("Document editing service did not return formsDataUrl");
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }*/
        
        // Save the file into LogicalDOC
        String sid = request.getParameter("sid");
        System.out.println("sid: " +sid);
        if (sid == null || sid.isEmpty()) {
        	System.out.println("sid: IS EMPTY OR NULL, treminate execution!");
        	return;
        }
        	
        Session session = SessionManager.get().get(sid);
        User ldUser = session.getUser();
        
        //long documentID = Long.parseLong(key);
        long documentID = Long.parseLong(key.substring(0, key.indexOf("-")));
        Document dldDoc = OnlyOfficeIndex.getDocument(documentID, ldUser);
        
        DocumentHistory transaction = new DocumentHistory();
		transaction.setFolderId(dldDoc.getFolder().getId());
		transaction.setUser(ldUser);
		transaction.setDocId(documentID);
		transaction.setUserId(ldUser.getId());
		transaction.setNotified(0);
		transaction.setComment("onlyoffice checkin");
		
		com.logicaldoc.core.document.DocumentManager ldDM = (com.logicaldoc.core.document.DocumentManager)Context.get().getBean("documentManager");
	
        try (InputStream is = new ByteArrayInputStream(byteArrayFile)) {
			ldDM.checkin(documentID, is, fileName, false, dldDoc, transaction);
			
			System.out.println("Operation completed");
		} catch (PersistenceException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}        
        
    } 

    // get byte array from stream
    private static byte[] getAllBytes(final InputStream is) throws IOException {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        byte[] buffer = new byte[BUFFER_SIZE];
        for (int len = is.read(buffer); len != -1; len = is.read(buffer)) {
            os.write(buffer, 0, len);
        }
        return os.toByteArray();
    }

    // download file from url
    private static byte[] getDownloadFile(final String url) throws Exception {
        if (url == null || url.isEmpty()) {
            throw new RuntimeException("Url argument is not specified");  // URL isn't specified
        }

        URL uri = new URL(url);
        java.net.HttpURLConnection connection = (java.net.HttpURLConnection) uri.openConnection();
        connection.setConnectTimeout(FILE_SAVE_TIMEOUT);
        InputStream stream = connection.getInputStream();  // get input stream of the file information from the URL

        int statusCode = connection.getResponseCode();

        if (statusCode != HttpServletResponse.SC_OK) {  // checking status code
            connection.disconnect();
            throw new RuntimeException("Document editing service returned status: " + statusCode);
        }

        if (stream == null) {
            connection.disconnect();
            throw new RuntimeException("Input stream is null");
        }

        return getAllBytes(stream);
    }
    
    // create a command request
    public static void commandRequest(final String method, final String key, final HashMap<String, String> meta) throws Exception {
    	
    	System.out.println("TrackManager commandRequest()");
    	
        String documentCommandUrl = ConfigManager.getProperty("files.docservice.url.site") + ConfigManager
                .getProperty("files.docservice.url.command");

        URL url = new URL(documentCommandUrl);
        System.out.println("documentCommandUrl: " +documentCommandUrl);
        java.net.HttpURLConnection connection = (java.net.HttpURLConnection) url.openConnection();

        HashMap<String, Object> params = new HashMap<String, Object>();
        params.put("c", method);
        params.put("key", key);

        if (meta != null) {
            params.put("meta", meta);
        }

        String headerToken = "";
        // check if a secret key to generate token exists or not
        if (OODocumentManager.tokenEnabled() && OODocumentManager.tokenUseForRequest()) {
            Map<String, Object> payloadMap = new HashMap<String, Object>();
            payloadMap.put("payload", params);
            headerToken = OODocumentManager.createToken(payloadMap);  // encode a payload object into a header token

            // add a header Authorization with a header token and Authorization prefix in it
            connection.setRequestProperty(DOCUMENT_JWT_HEADER.equals("")
                    ? "Authorization" : DOCUMENT_JWT_HEADER, "Bearer " + headerToken);

            String token = OODocumentManager.createToken(params);  // encode a payload object into a body token
            params.put("token", token);
        }

        Gson gson = new Gson();
        String bodyString = gson.toJson(params);        
        System.out.println("bodyString: " +bodyString);

        byte[] bodyByte = bodyString.getBytes(StandardCharsets.UTF_8);

        connection.setRequestMethod("POST");  // set the request method

        // set the Content-Type header
        connection.setRequestProperty("Content-Type", "application/json; charset=UTF-8");
        connection.setDoOutput(true); // set the doOutput field to true

        connection.connect();
        try (OutputStream os = connection.getOutputStream()) {
            os.write(bodyByte);  // write bytes to the output stream
        }
        InputStream stream = connection.getInputStream();  // get input stream

        if (stream == null) {
            throw new Exception("Could not get an answer");
        }

        String jsonString = ServiceConverter.convertStreamToString(stream);  // convert stream to json string
        System.out.println("jsonString: " + jsonString);
        connection.disconnect();

        JSONObject response = ServiceConverter.convertStringToJSON(jsonString);  // convert json string to json object
        if (!response.get("error").toString().equals("0")) {
            throw new Exception(response.toJSONString());
        }
    }

}
