package com.logicaldoc.core.metadata;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * DAO for <code>AttributeOption</code> handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public interface AttributeOptionDAO extends PersistentObjectDAO<AttributeOption> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static AttributeOptionDAO get() {
		return Context.get(AttributeOptionDAO.class);
	}
	
	/**
	 * This method deletes options.
	 * 
	 * @param setId ID of the attribute set
	 * @param attribute Name of the attribute (optional)
	 * 
	 * @throws PersistenceException Error in the database 
	 */
	public void deleteBySetIdAndAttribute(long setId, String attribute) throws PersistenceException;
	
	/**
	 * This method deletes the orphaned options of a given template
	 * 
	 * @param setId ID of the attribute set
	 * @param currentAttributes Names of the attributes of the actual template
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public void deleteOrphaned(long setId, Collection<String> currentAttributes) throws PersistenceException;

	/**
	 * This finds all the options for a given attribute. The list is ordered by
	 * position asc.
	 * 
	 * @param setId The attribute set id
	 * @param attribute The attribute name (Optional)
	 * 
	 * @return The ordered list of options
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<AttributeOption> findByAttribute(long setId, String attribute) throws PersistenceException;

	/**
	 * This finds all the options for a given attribute and groups them by category. The list is ordered by
	 * position asc.
	 * 
	 * @param setId The attribute set id
	 * @param attribute The attribute name
	 * 
	 * @return The map of opions, key is the category
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Map<String, List<AttributeOption>> findByAttributeAsMap(long setId, String attribute) throws PersistenceException;
	
	/**
	 * This finds all the options for a given attribute. The list is ordered by
	 * position asc.
	 * 
	 * @param setId The attribute set id
	 * @param attribute The attribute name (Optional)
	 * @param category The category (Optional)
	 * 
	 * @return The ordered list of options
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<AttributeOption> findByAttributeAndCategory(long setId, String attribute, String category) throws PersistenceException;
}