package com.logicaldoc.core.communication;

import java.util.List;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * This is a DAO service for MessageTemplate.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public interface MessageTemplateDAO extends PersistentObjectDAO<MessageTemplate> {

	/**
	 * Gets the object available in the application context
	 * 
	 * @return the instance of this object in the application context
	 */
	public static MessageTemplateDAO get() {
		return Context.get(MessageTemplateDAO.class);
	}

	/**
	 * Finds the templates by given language
	 * 
	 * @param language the language
	 * @param tenantId identifier of the tenant
	 * 
	 * @return collection of templates
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<MessageTemplate> findByLanguage(String language, long tenantId) throws PersistenceException;

	/**
	 * Finds the templates by given name, you may have the same name but for
	 * different languages.
	 * 
	 * @param name name of the template
	 * @param tenantId identifier of the tenant
	 * 
	 * @return collection of templates
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<MessageTemplate> findByName(String name, long tenantId) throws PersistenceException;

	/**
	 * Finds the template by the alternate key. If the template is not found for
	 * the specified language, the 'en' will be used instead.
	 * 
	 * @param name name of the template
	 * @param language the language, if null 'en' will be used instead
	 * @param tenantId the tenant
	 * 
	 * @return The found template or the 'en' if none was found
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public MessageTemplate findByNameAndLanguage(String name, String language, long tenantId)
			throws PersistenceException;

	/**
	 * Finds the templates by the type.
	 * 
	 * @param type type of the template
	 * @param language the language
	 * @param tenantId the tenant
	 * 
	 * @return the found templates
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<MessageTemplate> findByTypeAndLanguage(String type, String language, long tenantId)
			throws PersistenceException;
}