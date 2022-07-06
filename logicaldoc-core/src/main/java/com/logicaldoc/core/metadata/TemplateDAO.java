package com.logicaldoc.core.metadata;

import java.util.List;
import java.util.Set;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.security.Permission;

/**
 * This class is a DAO-service for document templates.
 * 
 * @author Marco Meschieri - LogicalDOC
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
	
	/**
	 * Finds all permissions of a user enabled on the specified template
	 * 
	 * @param templateId ID of the template
	 * @param userId ID of the user
	 * 
	 * @return Collection of enabled permissions
	 */
	public Set<Permission> getEnabledPermissions(long templateId, long userId);
	
	/**
	 * Returns if a template is writable for a user
	 * 
	 * @param templateId check this template
	 * @param userId privileges for this should be checked
	 * 
	 * @return if the user can edit the template
	 */
	public boolean isWriteEnable(long templateId, long userId);

	/**
	 * This method is looking up for read rights for a template and an user
	 * 
	 * @param templateId ID of the template
	 * @param userId ID of the user
	 * 
	 * @return if the user can access the template
	 */
	public boolean isReadEnable(long templateId, long userId);
}
