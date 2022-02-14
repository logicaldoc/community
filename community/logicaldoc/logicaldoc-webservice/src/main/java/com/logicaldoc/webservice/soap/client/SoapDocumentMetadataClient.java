package com.logicaldoc.webservice.soap.client;

import java.io.IOException;

import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSRight;
import com.logicaldoc.webservice.model.WSTemplate;
import com.logicaldoc.webservice.soap.DocumentMetadataService;

/**
 * Document Metadata Web Service client.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapDocumentMetadataClient extends SoapClient<DocumentMetadataService> implements DocumentMetadataService {

	public SoapDocumentMetadataClient(String endpoint) throws IOException {
		super(endpoint, DocumentMetadataService.class, -1, true, -1);
	}

	public SoapDocumentMetadataClient(String endpoint, int timeout) throws IOException {
		super(endpoint, DocumentMetadataService.class, -1, true, timeout);
	}

	@Override
	public WSTemplate[] listTemplates(String sid) throws Exception {
		return client.listTemplates(sid);
	}

	@Override
	public long storeTemplate(String sid, WSTemplate template) throws Exception {
		return client.storeTemplate(sid, template);
	}

	@Override
	public void deleteTemplate(String sid, long templateId) throws Exception {
		client.deleteTemplate(sid, templateId);
	}

	@Override
	public WSTemplate getTemplate(String sid, String name) throws Exception {
		return client.getTemplate(sid, name);
	}

	@Override
	public WSTemplate getTemplateById(String sid, long templateId) throws Exception {
		return client.getTemplateById(sid, templateId);
	}

	@Override
	public void setAttributeOptions(String sid, long setId, String attribute, String[] values) throws Exception {
		client.setAttributeOptions(sid, setId, attribute, values);
	}

	@Override
	public String[] getAttributeOptions(String sid, long setId, String attribute) throws Exception {
		return client.getAttributeOptions(sid, setId, attribute);
	}

	@Override
	public WSAttributeSet[] listAttributeSets(String sid) throws Exception {
		return client.listAttributeSets(sid);
	}

	@Override
	public WSAttributeSet getAttributeSet(String sid, String name) throws Exception {
		return client.getAttributeSet(sid, name);
	}

	@Override
	public WSAttributeSet getAttributeSetById(String sid, long setId) throws Exception {
		return client.getAttributeSetById(sid, setId);
	}

	@Override
	public long storeAttributeSet(String sid, WSAttributeSet attributeSet) throws Exception {
		return client.storeAttributeSet(sid, attributeSet);
	}

	@Override
	public void deleteAttributeSet(String sid, long setId) throws Exception {
		client.deleteAttributeSet(sid, setId);
	}

	@Override
	public boolean isTemplateReadable(String sid, long templateId) throws Exception {
		return client.isTemplateReadable(sid, templateId);
	}

	@Override
	public boolean isTemplateWritable(String sid, long templateId) throws Exception {
		return client.isTemplateWritable(sid, templateId);
	}

	@Override
	public void grantUserToTemplate(String sid, long templateId, long userId, int permissions) throws Exception {
		client.grantUserToTemplate(sid, templateId, userId, permissions);
	}

	@Override
	public void grantGroupToTemplate(String sid, long templateId, long groupId, int permissions) throws Exception {
		client.grantGroupToTemplate(sid, templateId, groupId, permissions);
	}

	@Override
	public WSRight[] getGrantedUsers(String sid, long groupId) throws Exception {
		return client.getGrantedUsers(sid, groupId);
	}

	@Override
	public WSRight[] getGrantedGroups(String sid, long groupId) throws Exception {
		return client.getGrantedGroups(sid, groupId);
	}
}
