package com.logicaldoc.core.metadata;

import java.util.List;
import java.util.Map;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.util.spring.Context;

/**
 * This class is a DAO-service for attribute sets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 7.5
 */
public interface AttributeSetDAO extends PersistentObjectDAO<AttributeSet> {

	/**
	 * Gets the object loaded in the execution context
	 * 
	 * @return the instance of this object in the execution context
	 */
	public static AttributeSetDAO get() {
		return Context.get(AttributeSetDAO.class);
	}
	
	/**
	 * This method finds a attribute set by name
	 * 
	 * @param name Name of the attribute set
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return AttributeSet with given name
	 * @throws PersistenceException Error in the database
	 */
	public AttributeSet findByName(String name, long tenantId) throws PersistenceException;

	/**
	 * This method finds a attribute set by type
	 * 
	 * @param type Type of the attribute set
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return AttributeSet with given type
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public List<AttributeSet> findByType(int type, long tenantId) throws PersistenceException;

	/**
	 * Retrieves all the attribute sets in a tenant
	 * 
	 * @param tenantId The tenant
	 * 
	 * @return Map ID-AttributeSet
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Map<Long, AttributeSet> load(long tenantId) throws PersistenceException;

	/**
	 * Gets the map of attributes defined in the given tenant or set
	 * 
	 * @param tenantId The tenant ID
	 * @param setId Optional set ID return map of attributes
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public Map<String, Attribute> findAttributes(long tenantId, Long setId) throws PersistenceException;
}