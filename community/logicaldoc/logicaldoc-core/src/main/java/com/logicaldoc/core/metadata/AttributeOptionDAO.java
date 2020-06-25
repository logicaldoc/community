package com.logicaldoc.core.metadata;

import java.util.Collection;
import java.util.List;

import com.logicaldoc.core.PersistentObjectDAO;

/**
 * DAO for <code>AttributeOption</code> handling.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public interface AttributeOptionDAO extends PersistentObjectDAO<AttributeOption> {

	/**
	 * This method deletes options.
	 * 
	 * @param setId ID of the attribute set
	 * @param attribute Name of the attribute (optional)
	 * 
	 * @return if all went ok
	 */
	public boolean deleteBySetIdAndAttribute(long setId, String attribute);
	
	/**
	 * This method deletes the orphaned options of a given template
	 * 
	 * @param setId ID of the attribute set
	 * @param currentAttributes Names of the attributes of the actual template
	 */
	public void deleteOrphaned(long setId, Collection<String> currentAttributes);

	/**
	 * This finds all the options for a given attribute. The list is ordered by
	 * position asc.
	 * 
	 * @param setId The attribute set id
	 * @param attribute The attribute name (Optional)
	 * 
	 * @return The ordered list of options
	 */
	public List<AttributeOption> findBySetIdAndAttribute(long setId, String attribute);
}