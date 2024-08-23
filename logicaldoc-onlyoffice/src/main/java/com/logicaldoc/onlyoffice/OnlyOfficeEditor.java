package com.logicaldoc.onlyoffice;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
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
 * This servlet is responsible for composing the OnlyOfficeEditor.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.0
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
			
			String sid = session.getSid();
			String docId = request.getParameter("docId");
			String fileName = request.getParameter("fileName");
			
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
			
			DocumentManager.init(request, response);
			
			//Setup document data
			Document myDoc = config.getDocument();
			myDoc.setFileType(dm.getExtension(fileName));
			myDoc.setTitle(fileName);
			myDoc.setUrl(DocumentManager.getDownloadUrl02(fileName, docId, sid, true));
			//myDoc.setKey(docId); 
			
			// Set the key to something unique
			long documentID = Long.parseLong(docId);
	        var dldDoc = IndexServlet.getDocument(documentID, session.getUser());
	        String xxx = docId +"-" +dldDoc.getVersion();
	        myDoc.setKey(xxx);			
			
			// set document permissions 
			//myDoc.getPermissions().setEdit(true);
			//myDoc.getPermissions().setModifyContentControl(true);
			
			String perm = om.writeValueAsString(myDoc.getPermissions());
			System.out.println("Permissions: \r\n" + perm);
						
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
			
	        // users data for mentions
	        List<Map<String, Object>> usersForMentions = Users.getUsersForMentions(user.getId());
	        List<Map<String, Object>> usersForProtect = Users.getUsersForProtect(user.getId());
	        List<Map<String, Object>> usersInfo = Users.getUsersInfo(user.getId());
			
			Gson gson = new Gson();
	        request.setAttribute("docserviceApiUrl", ConfigManager.getProperty("files.docservice.url.site")
	                + ConfigManager.getProperty("files.docservice.url.api"));			
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