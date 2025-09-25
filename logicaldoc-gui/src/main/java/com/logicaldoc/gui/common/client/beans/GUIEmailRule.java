package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

public class GUIEmailRule implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final int FIELD_SUBJECT = 0;

	public static final int FIELD_SENDER = 1;

	public static final int POLICY_CONTAINS = 0;

	public static final int POLICY_NOT_CONTAINS = 1;

	private int field = FIELD_SUBJECT;

	private int policy = POLICY_CONTAINS;

	private String expression;

	private GUIFolder target;

	public int getField() {
		return field;
	}

	public void setField(int field) {
		this.field = field;
	}

	public int getPolicy() {
		return policy;
	}

	public void setPolicy(int policy) {
		this.policy = policy;
	}

	public String getExpression() {
		return expression;
	}

	public void setExpression(String expression) {
		this.expression = expression;
	}

	public GUIFolder getTarget() {
		return target;
	}

	public void setTarget(GUIFolder target) {
		this.target = target;
	}
}