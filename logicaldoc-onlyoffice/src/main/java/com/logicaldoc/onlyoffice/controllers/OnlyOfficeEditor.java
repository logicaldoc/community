package com.logicaldoc.onlyoffice.controllers;

import java.net.URLEncoder;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.onlyoffice.entities.User;
import com.logicaldoc.onlyoffice.helpers.ConfigManager;
import com.logicaldoc.onlyoffice.helpers.FileUtility;
import com.logicaldoc.onlyoffice.helpers.OODocumentManager;
import com.logicaldoc.onlyoffice.helpers.Users;
import com.logicaldoc.onlyoffice.manager.OODocumentManagerImpl;
import com.logicaldoc.onlyoffice.manager.SettingsManagerImpl;
import com.logicaldoc.onlyoffice.manager.UrlMangerImpl;
import com.logicaldoc.onlyoffice.model.LDOOUSer;
import com.logicaldoc.onlyoffice.service.ConfigServiceImpl;
import com.onlyoffice.manager.security.DefaultJwtManager;
import com.onlyoffice.manager.security.JwtManager;
import com.onlyoffice.model.documenteditor.Config;
import com.onlyoffice.model.documenteditor.config.Document;
import com.onlyoffice.model.documenteditor.config.document.Type;
import com.onlyoffice.model.documenteditor.config.editorconfig.Mode;
import com.onlyoffice.service.documenteditor.config.ConfigService;

/**
 * This servlet is responsible for composing the OnlyOffice editor.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.1
 */
@WebServlet(name = "OnlyOfficeEditor", urlPatterns = {"/onlyoffice/editor"})
public class OnlyOfficeEditor extends HttpServlet {

	private static final long serialVersionUID = 7982449398452850052L;

	private static Logger log = LoggerFactory.getLogger(OnlyOfficeEditor.class);

	private SettingsManagerImpl settingsManager;

	private ObjectMapper objectMapper = new ObjectMapper();
	
	private OODocumentManagerImpl dm;

	private JwtManager jwtManager;
	
	@Override
	public void init() throws ServletException {
		super.init();
		this.settingsManager = new SettingsManagerImpl(ConfigManager.getProperty("files.docservice.url.site"), ConfigManager.getProperty("files.docservice.secret"));
		this.dm = new OODocumentManagerImpl(settingsManager);
		this.jwtManager = new DefaultJwtManager(settingsManager);
	}	
	
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) {
		
		try {
			Session session = OnlyOfficeEditor.validateSession(request);
			
			OODocumentManager.init(request, response);
			
			String sid = session.getSid();
			String docId = request.getParameter("docId");
			String fileName = request.getParameter("fileName");
			String fileExt = request.getParameter("fileExt");
			String folderIdRQ = request.getParameter("folderId");
			
			Boolean isEnableDirectUrl = true;
			try {
				isEnableDirectUrl = Boolean.valueOf(request.getParameter("directUrl"));
			} catch (Exception e) {
			}
			
			long folderId = 4;
			if (StringUtils.isNotEmpty(folderIdRQ)) 
				folderId = Long.parseLong(folderIdRQ);
			
			//com.logicaldoc.core.security.user.User userLD = session.getUser();
			
			User user = Users.getUser("uid-1");
			
	        if (fileExt != null) {
	        	System.out.println("fileExt: " +fileExt);
	            try {
	                // Create New demo document into LogicalDOC
	            	com.logicaldoc.core.document.Document ldDocument = OODocumentManager.createDemoLD(session.getUser(), fileExt, fileName, folderId, false);
	            	fileName = ldDocument.getFileName();
	                System.out.println("created fileName: " +fileName);

	                // redirect the request
	                response.sendRedirect(request.getContextPath() +"/onlyoffice/editor?fileName=" + URLEncoder.encode(fileName, "UTF-8") +"&docId=" +ldDocument.getId() +"&folderId=" +folderId + "&sid=" +sid);	                
	                return;
	            } catch (Exception ex) {
	                response.getWriter().write("Error: " + ex.getMessage());
	            }
	        }			
			
			UrlMangerImpl um = new UrlMangerImpl(settingsManager, request, sid);			
			ConfigService configService = new ConfigServiceImpl(dm, um, jwtManager, settingsManager);
			Config config = configService.createConfig(docId, Mode.EDIT, Type.DESKTOP);
			
			System.out.println("fileName: " +fileName);
			dm.setFileName(fileName);
			
			config.setDocumentType(dm.getDocumentType(fileName));
						
			
			//Setup document data
			Document myDoc = config.getDocument();
			myDoc.setFileType(dm.getExtension(fileName));
			myDoc.setTitle(fileName);
			myDoc.setUrl(OODocumentManager.getDownloadUrl02(fileName, docId, sid, true));
			//System.out.println("DownloadUrl: " +myDoc.getUrl());
			//myDoc.setKey(docId); 
			
			// Set the key to something unique
	        myDoc.setKey(docId +"-" + System.currentTimeMillis());		
						
			config.getEditorConfig().setCallbackUrl(OODocumentManager.getCallback02(fileName, docId, folderId, sid));
			// disable all customizations
			//config.getEditorConfig().setCustomization(null);
			
			// check if the Submit form button is displayed or not
			config.getEditorConfig().getCustomization().setSubmitForm(false);
			
			// Disable Autosave
			config.getEditorConfig().getCustomization().setAutosave(false);
			
			// Force Save Manually 
			config.getEditorConfig().getCustomization().setForcesave(true);		
			
			// Set the user for editing
			//com.onlyoffice.model.common.User edUser = new com.onlyoffice.model.common.User();
			LDOOUSer edUser = new LDOOUSer();
			edUser.setId(user.getId());
			edUser.setGroup("");
			edUser.setName(user.getName());
			edUser.setEmail(user.getEmail());
			config.getEditorConfig().setUser(edUser);
			
			// Setup createUrl to enable save as
			String createUrl = OODocumentManager.getCreateUrl(FileUtility.getFileType(fileName));
			createUrl += "&sid=" + sid +"&docId=" +docId +"&folderId=" +folderId;;
			config.getEditorConfig().setCreateUrl(!user.getId().equals("uid-0") ? createUrl : null);
			
			// rebuild the verification token
	        if (settingsManager.isSecurityEnabled()) {
	            config.setToken(jwtManager.createToken(config));
	        }
			
			try {
				String token = config.getToken();  
				//System.out.println("token: " +token); 
				String vtoken = jwtManager.verify(token); 
				System.out.println("vtoken: " +vtoken); 
			} catch (Exception e) { e.printStackTrace(); 
			}	

			request.setAttribute("config", objectMapper.writeValueAsString(config));
			
	        request.setAttribute("docserviceApiUrl", ConfigManager.getProperty("files.docservice.url.site")
	                + ConfigManager.getProperty("files.docservice.url.api"));
	        
	        // get an image and add it to the model
	        request.setAttribute("dataInsertImage", getInsertImage(isEnableDirectUrl));
	        
	        // get a document for comparison and add it to the model
	        request.setAttribute("dataDocument", getCompareFile(isEnableDirectUrl));
	        
	        // get recipients data for mail merging and add it to the model
	        request.setAttribute("dataSpreadsheet", getSpreadsheet(isEnableDirectUrl));
	        
	        // get user data for mentions and add it to the model
	        request.setAttribute("usersForMentions", getUserMentions(user.getId()));
	        
	        request.setAttribute("usersInfo", getUsersInfo(user.getId()));
	        
	        // get user data for protect and add it to the model
	        request.setAttribute("usersForProtect", getUserProtect(user.getId()));	
			
			request.getRequestDispatcher("/onlyoffice/editor.jsp").forward(request, response);
			
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println(e);
			handleError(response, e);
		}
	}

	private String getUserMentions(final String uid) throws JsonProcessingException {
		List<Map<String, Object>> usersForMentions = Users.getUsersForMentions(uid);		
		return !uid.equals("uid-0") ? objectMapper.writeValueAsString(usersForMentions) : null;
	}
	
	private String getUsersInfo(final String uid) throws JsonProcessingException {
		List<Map<String, Object>> usersInfo = Users.getUsersInfo(uid);
		return objectMapper.writeValueAsString(usersInfo);
	}	
	
	private String getUserProtect(final String uid) throws JsonProcessingException {
		List<Map<String, Object>> usersForProtect = Users.getUsersForProtect(uid);
		return !uid.equals("uid-0") ? objectMapper.writeValueAsString(usersForProtect) : null;
	}	

	// get recipients data for mail merging and add it to the model
	private String getSpreadsheet(Boolean isEnableDirectUrl) throws JsonProcessingException {
		Map<String, Object> dataSpreadsheet = new HashMap<>();
		dataSpreadsheet.put("fileType", "csv");
		dataSpreadsheet.put("url", OODocumentManager.getServerUrl(true) + "/onlyoffice/IndexServlet?"
		        + "type=csv");
		if (isEnableDirectUrl) {
		    dataSpreadsheet.put("directUrl", OODocumentManager.getServerUrl(false)
		            + "/onlyoffice/IndexServlet?type=csv");
		}
        return objectMapper.writeValueAsString(dataSpreadsheet);
	}

	// get a document that will be compared with the current document
	private String getCompareFile(Boolean isEnableDirectUrl) throws JsonProcessingException {
		
		Map<String, Object> dataDocument = new HashMap<>();
		dataDocument.put("fileType", "docx");
		dataDocument.put("url", OODocumentManager.getServerUrl(true) + "/onlyoffice/IndexServlet?type=assets&"
		        + "name=sample.docx");
		if (isEnableDirectUrl) {
		    dataDocument.put("directUrl", OODocumentManager.getServerUrl(false) + "/onlyoffice/IndexServlet?"
		            + "type=assets&name=sample.docx");
		}
		
        return objectMapper.writeValueAsString(dataDocument);
	}

	// get an image that will be inserted into the document
	private String getInsertImage(Boolean isEnableDirectUrl) throws JsonProcessingException {
		Map<String, Object> dataInsertImage = new HashMap<>();
		dataInsertImage.put("fileType", "png");
		dataInsertImage.put("url", OODocumentManager.getServerUrl(true) + "/onlyoffice/css/img/logo.png");
		if (isEnableDirectUrl) {
		    dataInsertImage.put("directUrl", OODocumentManager.getServerUrl(false) + "/onlyoffice/css/img/logo.png");
		}
		
        return objectMapper.writeValueAsString(dataInsertImage)
                .substring(1, objectMapper.writeValueAsString(dataInsertImage).length() - 1);
	}	

	private void handleError(HttpServletResponse response, Throwable e) {
		String message = e.getMessage();
		log.error(message, e);
		try {
			response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
		} catch (Exception t) {
			// Nothing to do
		}
	}
    
	public static Session validateSession(HttpServletRequest request) throws InvalidSessionServerException {
		String sid = SessionManager.get().getSessionId(request);
		Session session = SessionManager.get().get(sid);
		if (session == null)
			throw new InvalidSessionServerException("Invalid Session");
		if (!SessionManager.get().isOpen(sid))
			throw new InvalidSessionServerException("Invalid or Expired Session");
		SessionManager.get().renew(sid);
		return session;
	}    

}