package com.logicaldoc.webservice.doc;

/**
 * To display a java-style name? for example, orderId =&gt; Order ID
 */
public abstract class JavaNameDisplayStrategy {

	public abstract String displayElementName(String stubName);

	public abstract String displayElementType(Class<?> type);

	/**
	 * "HelloOrder" -&gt; "Hello Order", Mainly for Class Inheritance display
	 * 
	 * @param clazz The class to treat
	 * 
	 * @return the pretty printed name
	 */
	public abstract String displayClassName(Class<?> clazz);

}
