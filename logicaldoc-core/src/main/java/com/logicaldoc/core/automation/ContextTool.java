package com.logicaldoc.core.automation;

import java.util.List;

import com.logicaldoc.util.spring.Context;

/**
 * Utility methods to access the Application context from inside the Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.1
 */
@AutomationDictionary
public class ContextTool {

	/**
	 * Gets a specific bean instance
	 * 
	 * @param id identifier of the bean
	 * 
	 * @return the instance
	 */
	public Object getBean(String id) {
		return Context.get(id);
	}

	/**
	 * Gets the collection of all the identifiers in the context
	 * 
	 * @return list of bean identifiers
	 */
	public List<String> getBeanIds() {
		return Context.get().getBeanIds();
	}
}