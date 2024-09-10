package com.logicaldoc.webservice.rest.client;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.rest.DocumentMetadataService;

public class RestDocumentMetadataClient extends AbstractRestClient<DocumentMetadataService> {

	protected static Logger log = LoggerFactory.getLogger(RestDocumentMetadataClient.class);

	public RestDocumentMetadataClient(String endpoint, String apiKey) {
		this(endpoint, apiKey, -1);
	}

	public RestDocumentMetadataClient(String endpoint, String apiKey, int timeout) {
		super(DocumentMetadataService.class, endpoint, apiKey, timeout);
	}

	public void setAttributeOptions(long setId, String attribute, List<WSAttributeOption> options)
			throws WebserviceException, PersistenceException {
		proxy.setAttributeOptions(setId, attribute, options);
	}

	public void setAttributeOptionsPOST(long setId, String attribute, List<WSAttributeOption> options)
			throws WebserviceException, PersistenceException {
		proxy.setAttributeOptionsPOST(setId, attribute, options);
	}

	public long storeAttributeSet(WSAttributeSet attributeSet) throws WebserviceException, PersistenceException {
		return proxy.storeAttributeSet(attributeSet);
	}

	public long storeTemplate(WSTemplate template)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return proxy.storeTemplate(template);
	}

	public WSAttributeSet getAttributeSetById(long setId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getAttributeSetById(setId);
	}

	public WSAttributeSet getAttributeSet(String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getAttributeSet(name);
	}

	public WSTemplate getTemplate(String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getTemplate(name);
	}

	public WSTemplate getTemplateById(long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getTemplateById(templateId);
	}

	public List<String> getAttributeOptions(long setId, String attribute)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.getAttributeOptions(setId, attribute);
	}

	public List<WSAttributeSet> listAttributeSets()
			throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.listAttributeSets();
	}

	public void deleteAttributeSet(long setId) throws WebserviceException, PersistenceException {
		proxy.deleteAttributeSet(setId);
	}

	public void deleteTemplate(long templateId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		proxy.deleteTemplate(templateId);
	}

	public List<WSTemplate> listTemplates() throws AuthenticationException, WebserviceException, PersistenceException {
		return proxy.listTemplates();
	}
}