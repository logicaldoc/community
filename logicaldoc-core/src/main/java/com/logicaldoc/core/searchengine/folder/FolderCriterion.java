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

	public static final String OPERATOR_EQUALS = "equals";

	public static final String OPERATOR_NOTEQUAL = "notequal";

	public static final String OPERATOR_CONTAINS = "contains";

	public static final String OPERATOR_NOTCONTAINS = "notcontains";

	public static final String OPERATOR_BEGINSWITH = "beginswith";

	public static final String OPERATOR_ENDSWITH = "endswith";

	public static final String OPERATOR_GREATER = "greaterthan";

	public static final String OPERATOR_LESSER = "lessthan";

	public static final String OPERATOR_NULL = "null";

	public static final String OPERATOR_NOTNULL = "notnull";

	public static final String OPERATOR_IN = "in";

	public static final String OPERATOR_INORSUBFOLDERS = "inorsubfolders";

	private String composition = "and";

	private String operator = OPERATOR_EQUALS;

	public static final int TYPE_LANGUAGE = 100;

	public static final int TYPE_FOLDER = 101;

	public static final int TYPE_TEMPLATE = 102;

	private String stringValue;

	private Date dateValue;

	private Long longValue;

	private Double doubleValue;

	private int type = Attribute.TYPE_INT;

	private static Map<String, Integer> typeBinding = new HashMap<>();

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

		switch (getType()) {
		case Attribute.TYPE_INT:
		case Attribute.TYPE_BOOLEAN:
		case Attribute.TYPE_USER:
		case TYPE_FOLDER:
		case TYPE_TEMPLATE:
			return getLongValue() == null;
		case Attribute.TYPE_DOUBLE:
			return getDoubleValue() == null;
		case Attribute.TYPE_DATE:
			return getDateValue() == null;
		default:
			return StringUtils.isEmpty(getStringValue());
		}
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
			if (value instanceof Integer integer)
				setLongValue(integer.longValue());
			else
				setLongValue((Long) value);
			break;
		case Attribute.TYPE_DOUBLE:
			if (value instanceof Double doubleVal)
				setDoubleValue(doubleVal);
			else if (value instanceof Long longVal)
				setDoubleValue(longVal.doubleValue());
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
		default:
			// do noting
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