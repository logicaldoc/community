package com.logicaldoc.core.automation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Utility methods to handle collections
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.4
 */
@AutomationDictionary
public class CollectionTool {

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
			return new ArrayList<>();
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
			return new Object[0];
		else
			return list.toArray();
	}
}