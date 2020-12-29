package com.logicaldoc.core.searchengine.folder;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.metadata.Attribute;

public class FolderCriterion implements Serializable {

	private static final String FIELD_TAGS = "tags";

	private static final long serialVersionUID = 1L;

	private String field = "id";

	private String fieldName = "id";

	public final static String OPERATOR_EQUALS = "equals";

	public final static String OPERATOR_NOTEQUAL = "notequal";

	public final static String OPERATOR_CONTAINS = "contains";

	public final static String OPERATOR_NOTCONTAINS = "notcontains";

	public final static String OPERATOR_BEGINSWITH = "beginswith";
	
	public final static String OPERATOR_ENDSWITH = "endswith";
	
	public final static String OPERATOR_GREATER = "greaterthan";

	public final static String OPERATOR_LESSER = "lessthan";

	public final static String OPERATOR_NULL = "null";

	public final static String OPERATOR_NOTNULL = "notnull";

	public final static String OPERATOR_IN = "in";

	public final static String OPERATOR_INORSUBFOLDERS = "inorsubfolders";

	private String composition = "and";

	private String operator = OPERATOR_EQUALS;

	public final static int TYPE_LANGUAGE = 100;

	public final static int TYPE_FOLDER = 101;

	public final static int TYPE_TEMPLATE = 102;

	private String stringValue;

	private Date dateValue;

	private Long longValue;

	private Double doubleValue;

	private int type = Attribute.TYPE_INT;

	private static Map<String, Integer> typeBinding = new HashMap<String, Integer>();

	private boolean showFolderSelector = false;

	private String parentPathDescr;

	private boolean extendedAttribute = false;

	static {
		typeBinding.put("creation", Attribute.TYPE_DATE);
		typeBinding.put("lastModified", Attribute.TYPE_DATE);

		typeBinding.put("id", Attribute.TYPE_INT);

		typeBinding.put("folder", TYPE_FOLDER);

		typeBinding.put("template", TYPE_TEMPLATE);
	}

	public void setField(String field) {
		this.field = field;
		if (field.contains(" type:")) {
			String name = field.substring(0, field.lastIndexOf(" type:"));
			this.fieldName = name;

			if (typeBinding.containsKey(name)) {
				this.field = fieldName;
				this.type = typeBinding.get(name);
			} else {
				this.type = Integer.parseInt(field.substring(field.lastIndexOf(":") + 1));
				this.extendedAttribute = true;
			}
		} else if (typeBinding.containsKey(field)) {
			this.type = typeBinding.get(field);
			this.fieldName = field;
		} else {
			this.type = Attribute.TYPE_STRING;
			this.fieldName = field;
		}
	}

	public boolean isExtendedAttribute() {
		return extendedAttribute;
	}

	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

	public String getColumnName() {
		if (getType() == TYPE_TEMPLATE)
			return "ld_templateid";
		else if (getType() == TYPE_FOLDER)
			return "ld_parentid";
		else if (getFieldName().equalsIgnoreCase(FIELD_TAGS))
			return "ld_tgs";
		else
			return "ld_" + getFieldName().toLowerCase();
	}

	public String getField() {
		return field;
	}

	public String getComposition() {
		return composition;
	}

	public void setComposition(String composition) {
		this.composition = composition;
	}

	public String getOperator() {
		return operator;
	}

	public void setOperator(String operator) {
		this.operator = operator;
	}

	public boolean isEmpty() {
		if (OPERATOR_NULL.equals(operator))
			return false;

		if ((getType() == Attribute.TYPE_STRING && StringUtils.isEmpty(getStringValue().trim()))
				|| (getType() == Attribute.TYPE_INT && getLongValue() == null)
				|| (getType() == Attribute.TYPE_DOUBLE && getDoubleValue() == null)
				|| (getType() == Attribute.TYPE_DATE && getDateValue() == null)
				|| (getType() == Attribute.TYPE_BOOLEAN && getLongValue() == null)
				|| (getType() == Attribute.TYPE_USER && getLongValue() == null)
				|| (getType() == Attribute.TYPE_FOLDER && getLongValue() == null)
				|| (getType() == TYPE_FOLDER && getLongValue() == null)
				|| (getType() == TYPE_TEMPLATE && getLongValue() == null)
				|| (getType() == TYPE_LANGUAGE && getStringValue() == null))
			return true;

		return false;
	}

	public void setValue(Serializable value) {
		if (value == null)
			return;

		switch (getType()) {
		case Attribute.TYPE_STRING:
			if (fieldName.equalsIgnoreCase(FIELD_TAGS))
				setStringValue("," + (String) value + ",");
			else
				setStringValue((String) value);
			break;
		case Attribute.TYPE_INT:
		case Attribute.TYPE_FOLDER:
		case Attribute.TYPE_USER:
		case Attribute.TYPE_BOOLEAN:
			if (value instanceof Integer)
				setLongValue(((Integer) value).longValue());
			else
				setLongValue((Long) value);
			break;
		case Attribute.TYPE_DOUBLE:
			if (value instanceof Double)
				setDoubleValue((Double) value);
			else if (value instanceof Long)
				setDoubleValue(((Long) value).doubleValue());
			else
				setDoubleValue(((Float) value).doubleValue());
			break;
		case Attribute.TYPE_DATE:
			setDateValue((Date) value);
			break;
		case TYPE_FOLDER:
			setLongValue((Long) value);
			break;
		case TYPE_TEMPLATE:
			setLongValue((Long) value);
			break;
		case TYPE_LANGUAGE:
			setStringValue((String) value);
			break;
		}
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public String getStringValue() {
		return stringValue;
	}

	public void setStringValue(String stringValue) {
		this.stringValue = stringValue;
	}

	public Date getDateValue() {
		return dateValue;
	}

	public Date getSqlDateValue() {
		if (dateValue != null)
			return new java.sql.Date(dateValue.getTime());
		else
			return null;
	}

	public void setDateValue(Date dateValue) {
		this.dateValue = dateValue;
	}

	public boolean isShowFolderSelector() {
		return showFolderSelector;
	}

	public void setShowFolderSelector(boolean showFolderSelector) {
		this.showFolderSelector = showFolderSelector;
	}

	public String getParentPathDescr() {
		return parentPathDescr;
	}

	public void setParentPathDescr(String parentPathDescr) {
		this.parentPathDescr = parentPathDescr;
	}

	public Long getLongValue() {
		return longValue;
	}

	public void setLongValue(Long longValue) {
		this.longValue = longValue;
	}

	public Double getDoubleValue() {
		return doubleValue;
	}

	public void setDoubleValue(Double doubleValue) {
		this.doubleValue = doubleValue;
	}

	public void setExtendedAttribute(boolean extendedAttribute) {
		this.extendedAttribute = extendedAttribute;
	}
}