package com.logicaldoc.onlyoffice.controllers;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.onlyoffice.entities.User;
import com.logicaldoc.onlyoffice.helpers.ConfigManager;
import com.logicaldoc.onlyoffice.helpers.DocumentManager;
import com.logicaldoc.onlyoffice.helpers.FileUtility;
import com.logicaldoc.onlyoffice.helpers.Users;
import com.logicaldoc.onlyoffice.manager.DocumentManagerImpl;
import com.logicaldoc.onlyoffice.manager.SettingsManagerImpl;
import com.logicaldoc.onlyoffice.manager.UrlMangerImpl;
import com.logicaldoc.onlyoffice.model.LDOOUSer;
import com.logicaldoc.onlyoffice.service.ConfigServiceImpl;
import com.onlyoffice.manager.security.DefaultJwtManager;
import com.onlyoffice.manager.security.JwtManager;
import com.onlyoffice.manager.settings.SettingsManager;
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
@WebServlet(name = "OnlyOfficeEditor", urlPatterns = {"/onlyoffice/OnlyOfficeEditor"})
public class OnlyOfficeEditor extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(OnlyOfficeEditor.class);

	private SettingsManagerImpl settingsManager;

	private ObjectMapper om = new ObjectMapper();
	
	private ConfigService configService;
	
	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) {
		
		try {
			Session session = OnlyOfficeEditor.validateSession(request);
			
			DocumentManager.init(request, response);
			
			String sid = session.getSid();
			String docId = request.getParameter("docId");
			String fileName = request.getParameter("fileName");
			
			Boolean isEnableDirectUrl = true;
			try {
				isEnableDirectUrl = Boolean.valueOf(request.getParameter("directUrl"));
			} catch (Exception e) {
			}
			
			//com.logicaldoc.core.security.user.User userLD = session.getUser();
			
			User user = Users.getUser("uid-1");
			
			settingsManager = new SettingsManagerImpl(ConfigManager.getProperty("files.docservice.url.site"), ConfigManager.getProperty("files.docservice.secret"));
			DocumentManagerImpl dm = new DocumentManagerImpl(settingsManager);
			UrlMangerImpl um = new UrlMangerImpl(settingsManager, request, sid);
			JwtManager jwtManager = this.jwtManager(settingsManager);
			configService = new ConfigServiceImpl(dm, um, jwtManager, settingsManager);
			Config config = configService.createConfig(docId, Mode.EDIT, Type.DESKTOP);
			
			dm.setFileName(fileName);
			
			config.setDocumentType(dm.getDocumentType(fileName));
						
			
			//Setup document data
			Document myDoc = config.getDocument();
			myDoc.setFileType(dm.getExtension(fileName));
			myDoc.setTitle(fileName);
			myDoc.setUrl(DocumentManager.getDownloadUrl02(fileName, docId, sid, true));
			//myDoc.setKey(docId); 
			
			// Set the key to something unique
			//long documentID = Long.parseLong(docId);
//	        var dldDoc = OnlyOfficeIndex.getDocument(documentID, session.getUser());
//	        String xxx = docId +"-" +dldDoc.getVersion();
	        myDoc.setKey(docId +"-" + System.currentTimeMillis());		
						
			config.getEditorConfig().setCallbackUrl(DocumentManager.getCallback02(fileName, docId, sid));
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
			String createUrl = DocumentManager.getCreateUrl(FileUtility.getFileType(fileName));
			createUrl += "?sid=" + sid;
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

			request.setAttribute("config", om.writeValueAsString(config));
			
	        // an image that will be inserted into the document
	        Map<String, Object> dataInsertImage = new HashMap<>();
	        dataInsertImage.put("fileType", "png");
	        dataInsertImage.put("url", DocumentManager.getServerUrl(true) + "/css/img/logo.png");
	        if (isEnableDirectUrl) {
	            dataInsertImage.put("directUrl", DocumentManager.getServerUrl(false) + "/css/img/logo.png");
	        }

	        // a document that will be compared with the current document
	        Map<String, Object> dataDocument = new HashMap<>();
	        dataDocument.put("fileType", "docx");
	        dataDocument.put("url", DocumentManager.getServerUrl(true) + "/onlyoffice/IndexServlet?type=assets&"
	                + "name=sample.docx");
	        if (isEnableDirectUrl) {
	            dataDocument.put("directUrl", DocumentManager.getServerUrl(false) + "/onlyoffice/IndexServlet?"
	                    + "type=assets&name=sample.docx");
	        }

	        // recipients data for mail merging
	        Map<String, Object> dataSpreadsheet = new HashMap<>();
	        dataSpreadsheet.put("fileType", "csv");
	        dataSpreadsheet.put("url", DocumentManager.getServerUrl(true) + "/onlyoffice/IndexServlet?"
	                + "type=csv");
	        if (isEnableDirectUrl) {
	            dataSpreadsheet.put("directUrl", DocumentManager.getServerUrl(false)
	                    + "/onlyoffice/IndexServlet?type=csv");
	        }			
			
	        // users data for mentions
	        List<Map<String, Object>> usersForMentions = Users.getUsersForMentions(user.getId());
	        List<Map<String, Object>> usersForProtect = Users.getUsersForProtect(user.getId());
	        List<Map<String, Object>> usersInfo = Users.getUsersInfo(user.getId());
			
			Gson gson = new Gson();
	        request.setAttribute("docserviceApiUrl", ConfigManager.getProperty("files.docservice.url.site")
	                + ConfigManager.getProperty("files.docservice.url.api"));
	        
	        request.setAttribute("dataInsertImage",  gson.toJson(dataInsertImage).substring(1, gson.toJson(dataInsertImage).length() - 1));
	        request.setAttribute("dataDocument",  gson.toJson(dataDocument));
	        request.setAttribute("dataSpreadsheet", gson.toJson(dataSpreadsheet));
	        
	        request.setAttribute("usersForMentions", !user.getId().equals("uid-0") ? gson.toJson(usersForMentions) : null);
	        request.setAttribute("usersInfo", gson.toJson(usersInfo));
	        request.setAttribute("usersForProtect", !user.getId().equals("uid-0") ? gson.toJson(usersForProtect) : null);			
			
			request.getRequestDispatcher("/onlyoffice/editor.jsp").forward(request, response);
			
		} catch (Exception e) {
			handleError(response, e);
		}
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
	
    @Bean
    public JwtManager jwtManager(final SettingsManager settingsManager) {
        return new DefaultJwtManager(settingsManager);
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