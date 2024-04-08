package com.logicaldoc.webservice.soap.client;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.soap.DocumentMetadataService;

/**
 * Document Metadata Web Service client.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapDocumentMetadataClient extends SoapClient<DocumentMetadataService> implements DocumentMetadataService {

	public SoapDocumentMetadataClient(String endpoint) {
		super(endpoint, DocumentMetadataService.class, -1, true, -1);
	}

	public SoapDocumentMetadataClient(String endpoint, int timeout) {
		super(endpoint, DocumentMetadataService.class, -1, true, timeout);
	}

	@Override
	public List<WSTemplate> listTemplates(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.listTemplates(sid);
	}

	@Override
	public long storeTemplate(String sid, WSTemplate template)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		return client.storeTemplate(sid, template);
	}

	@Override
	public void deleteTemplate(String sid, long templateId)
			throws AuthenticationException, PermissionException, WebserviceException, PersistenceException {
		client.deleteTemplate(sid, templateId);
	}

	@Override
	public WSTemplate getTemplate(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getTemplate(sid, name);
	}

	@Override
	public WSTemplate getTemplateById(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getTemplateById(sid, templateId);
	}

	@Override
	public void setAttributeOptions(String sid, long setId, String attribute, List<WSAttributeOption> options)
			throws WebserviceException, PersistenceException {
		client.setAttributeOptions(sid, setId, attribute, options);
	}

	@Override
	public List<String> getAttributeOptions(String sid, long setId, String attribute)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getAttributeOptions(sid, setId, attribute);
	}

	@Override
	public List<WSAttributeOption> getAttributeOptionsByCategory(String sid, long setId, String attribute,
			String category) throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getAttributeOptionsByCategory(sid, setId, attribute, category);
	}

	@Override
	public List<WSAttributeSet> listAttributeSets(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.listAttributeSets(sid);
	}

	@Override
	public WSAttributeSet getAttributeSet(String sid, String name)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getAttributeSet(sid, name);
	}

	@Override
	public WSAttributeSet getAttributeSetById(String sid, long setId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.getAttributeSetById(sid, setId);
	}

	@Override
	public long storeAttributeSet(String sid, WSAttributeSet attributeSet)
			throws WebserviceException, PersistenceException {
		return client.storeAttributeSet(sid, attributeSet);
	}

	@Override
	public void deleteAttributeSet(String sid, long setId) throws WebserviceException, PersistenceException {
		client.deleteAttributeSet(sid, setId);
	}

	@Override
	public boolean isReadable(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isReadable(sid, templateId);
	}

	@Override
	public boolean isWritable(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		return client.isWritable(sid, templateId);
	}

	@Override
	public void addAttributeOption(String sid, long setId, String attribute, WSAttributeOption wsoption)
			throws AuthenticationException, WebserviceException, PersistenceException {
		client.addAttributeOption(sid, setId, attribute, wsoption);
	}

	@Override
	public List<WSAccessControlEntry> getAccessControlList(String sid, long templateId)
			throws AuthenticationException, WebserviceException, PersistenceException, PermissionException {
		return client.getAccessControlList(sid, templateId);
	}

	@Override
	public void setAccessControlList(String sid, long templateId, List<WSAccessControlEntry> acl)
			throws PersistenceException, PermissionException, AuthenticationException, WebserviceException {
		client.setAccessControlList(sid, templateId, acl);
	}
}