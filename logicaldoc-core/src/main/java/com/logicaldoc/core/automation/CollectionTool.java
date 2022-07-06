package com.logicaldoc.core.automation;

import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility methods to handle collections
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.4
 */
@AutomationDictionary
public class CollectionTool {

	protected static Logger log = LoggerFactory.getLogger(CollectionTool.class);

	/**
	 * Converts an array into a list
	 * 
	 * @param array the array to process
	 * 
	 * @return the list
	 * 
	 * @since 8.7.4
	 */
	public List<Object> toList(Object[] array) {
		if (array == null)
			return null;
		else
			return Arrays.asList(array);
	}

	/**
	 * Converts an array into a list
	 * 
	 * @param list the list to process
	 * 
	 * @return the array
	 * 
	 * @since 8.7.4
	 */
	public Object[] toArray(List<Object> list) {
		if (list == null)
			return null;
		else
			return list.toArray();
	}
}