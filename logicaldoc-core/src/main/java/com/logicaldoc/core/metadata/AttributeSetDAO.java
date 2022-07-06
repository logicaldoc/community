package com.logicaldoc.core.metadata;

import java.util.List;
import java.util.Map;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * This class is a DAO-service for attribute sets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 7.5
 */
public interface AttributeSetDAO extends PersistentObjectDAO<AttributeSet> {

	/**
	 * This method finds a attribute set by name
	 * 
	 * @param name Name of the attribute set
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return AttributeSet with given name
	 */
	public AttributeSet findByName(String name, long tenantId);

	/**
	 * This method finds a attribute set by type
	 * 
	 * @param type Type of the attribute set
	 * @param tenantId ID of the owning tenant
	 * 
	 * @return AttributeSet with given type
	 */
	public List<AttributeSet> findByType(int type, long tenantId);

	/**
	 * Retrieves all the attribute sets in a tenant
	 * 
	 * @param tenantId The tenant
	 * @return Map ID-AttributeSet
	 */
	public Map<Long, AttributeSet> load(long tenantId);

	/**
	 * Gets the map of attributes defined in the given tenant or set
	 * 
	 * @param tenantId The tenant ID
	 * @param setId Optional set ID
	 * @return map of attributes
	 */
	public Map<String, Attribute> findAttributes(long tenantId, Long setId);
}