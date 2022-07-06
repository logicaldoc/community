package com.logicaldoc.webservice.doc.model;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

/**
 * 
 * Imagine a stub = an @XMLElement. It's a tree-like data structure
 * 
 */
public class Stub {

	/**
	 * as "order" in @XmlType(name = "order")
	 */
	private String stubName;

	/**
	 * the java type of a element, such as "Order.class"
	 */
	private Class<?> type;

	private boolean required;

	private boolean multiOccurs;

	/**
	 * Child elements, such as {orderId, orderDate}
	 */
	private List<Stub> childStubs = new ArrayList<Stub>();

	/**
	 * if parent stub's type = Product, is it FunProduct or NotFunProduct?
	 */
	private Class<?> subTypeOfParentStub;

	private String description = "";

	public String getStubName() {
		return stubName;
	}

	public void setStubName(String stubName) {
		this.stubName = stubName;
	}

	public boolean isRequired() {
		return required;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	public List<Stub> getChildStubs() {
		return childStubs;
	}

	public void addChild(Stub e) {
		childStubs.add(e);

	}

	public Class<?> getType() {
		return type;
	}

	public void setType(Class<?> type) {
		this.type = type;
	}

	public Class<?> getSubTypeOfParentStub() {
		return subTypeOfParentStub;
	}

	public void setSubTypeOfParentStub(Class<?> subTypeOfDeclaringStub) {
		this.subTypeOfParentStub = subTypeOfDeclaringStub;
	}

	public boolean isMultiOccurs() {
		return multiOccurs;
	}

	public void setMultiOccurs(boolean multiOccurs) {
		this.multiOccurs = multiOccurs;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(stubName).append(type).append(required)
				.append(childStubs).toString();
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

}
