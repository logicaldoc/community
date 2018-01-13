package com.logicaldoc.webservice.doc;

/**
 * To display a java-style name? for example, orderId => Order ID 
 */
public abstract class JavaNameDisplayStrategy {

    public abstract String displayElementName(String stubName);

    public abstract String displayElementType(Class<?> type);

    /**
     * "HelloOrder" -> "Hello Order", Mainly for Class Inheritance display
     * 
     * @param className
     * @return
     */
    public abstract String displayClassName(Class<?> clazz);

}
