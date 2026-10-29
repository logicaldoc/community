package com.logicaldoc.core.searchengine.folder;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

    private int type = Attribute.Type.INTEGER.ordinal();

    private static Map<String, Integer> typeBinding = new HashMap<>();

    private boolean showFolderSelector = false;

    private String parentPathDescr;

    private boolean extendedAttribute = false;

    static {
        typeBinding.put("creation", Attribute.Type.DATE.ordinal());
        typeBinding.put("lastModified", Attribute.Type.DATE.ordinal());

        typeBinding.put("id", Attribute.Type.INTEGER.ordinal());

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
            this.type = Attribute.Type.STRING.ordinal();
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
            return "ld_%s".formatted(getFieldName().toLowerCase());
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

        if (type == Attribute.Type.STRING.ordinal())
            return StringUtils.isEmpty(getStringValue());
        else if (type == Attribute.Type.DOUBLE.ordinal())
            return getDoubleValue() == null;
        else if (type == Attribute.Type.DATE.ordinal())
            return getDateValue() == null;
        else
            return getLongValue() == null;
    }

    public void setValue(Serializable value) {
        if (value == null)
            return;

        if (type == Attribute.Type.STRING.ordinal()) {
            if (fieldName.equalsIgnoreCase(FIELD_TAGS))
                setStringValue(",%s,".formatted(value));
            else
                setStringValue((String) value);
        } else if (type == Attribute.Type.INTEGER.ordinal() || type == Attribute.Type.FOLDER.ordinal()
                || type == Attribute.Type.DOCUMENT.ordinal() || type == Attribute.Type.USER.ordinal()
                || type == Attribute.Type.BOOLEAN.ordinal()) {
            if (value instanceof Integer integer)
                setLongValue(integer.longValue());
            else
                setLongValue((Long) value);
        } else if (type == Attribute.Type.DOUBLE.ordinal()) {
            switch (value) {
                case Double doubleVal -> setDoubleValue(doubleVal);
                case Long longVal -> setDoubleValue(longVal.doubleValue());
                case Float floatVal -> setDoubleValue(floatVal.doubleValue());
                default -> setDoubleValue(null);
            }
        } else if (type == Attribute.Type.DATE.ordinal()) {
            setDateValue((Date) value);
        } else if (type == TYPE_FOLDER || type == TYPE_TEMPLATE) {
            setLongValue((Long) value);
        } else if (type == TYPE_LANGUAGE) {
            setStringValue((String) value);
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

    @Override
    public String toString() {
        return new ReflectionToStringBuilder(this, ToStringStyle.NO_CLASS_NAME_STYLE) {
            @Override
            protected boolean accept(Field field) {
                try {
                    Object value = field.get(getObject());
                    return super.accept(field) && value != null && StringUtils.isNotEmpty(value.toString())
                            && !field.getName().equals("field");
                } catch (IllegalAccessException | IllegalArgumentException | SecurityException e) {
                    return false;
                }
            }
        }.toString();
    }
}