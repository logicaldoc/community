package com.logicaldoc.core.metadata;

import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * This class is a DAO-service for document templates.
 * 
 * @author Marco Meschieri - Logical Objects
 * @version 1.0
 */
public interface TemplateDAO extends PersistentObjectDAO<Template> {
	/**
	 * This method finds a template by name
	 * 
	 * @param name Name of the template
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return Template with given name
	 */
	public Template findByName(String name, long tenantId);

	/**
	 * Counts the total number of documents using this template
	 * 
	 * @param id The template ID
	 * @return the documents count
	 */
	public int countDocs(long id);

	/**
	 * This method finds a template by type
	 * 
	 * @param type Type of the template
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return Template with given type
	 */
	public List<Template> findByType(int type, long tenantId);
}
