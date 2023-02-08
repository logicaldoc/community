package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.OperatorId;

/**
 * Fake Datasource to populate a filter builder for folder searches.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.1
 */
public class FolderFieldsDS extends DataSource {

	private static final String TYPE = "type:";

	public FolderFieldsDS(GUITemplate template) {
		setClientOnly(true);

		/*
		 * Define default fields
		 */
		DataSourceTextField folderName = new DataSourceTextField("name", I18N.message("name"));
		folderName.setValidOperators(OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS, OperatorId.EQUALS,
				OperatorId.NOT_EQUAL);

		DataSourceTextField description = new DataSourceTextField("description", I18N.message("description"));
		description.setValidOperators(OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS, OperatorId.EQUALS,
				OperatorId.NOT_EQUAL);

		DataSourceIntegerField id = new DataSourceIntegerField("id", I18N.message("id"));
		id.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN, OperatorId.EQUALS, OperatorId.NOT_EQUAL);

		DataSourceTextField creator = new DataSourceTextField("creator", I18N.message("creator"));
		creator.setValidOperators(OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS, OperatorId.EQUALS,
				OperatorId.NOT_EQUAL);

		DataSourceDateTimeField created = new DataSourceDateTimeField("creation", I18N.message("createdon"));
		created.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN);

		DataSourceTextField tags = new DataSourceTextField("tags", I18N.message("tags"));
		tags.setValidOperators(OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS);

		DataSourceIntegerField tmplt = new DataSourceIntegerField("template", I18N.message("template"));
		tmplt.setValidOperators(OperatorId.IS_NULL);

		setFields(id, folderName, description, created, creator, tags, tmplt);

		/*
		 * Define extended attributes
		 */
		if (template != null && template.getAttributes() != null)
			for (GUIAttribute attribute : template.getAttributes()) {
				if (attribute.isHidden())
					continue;
				addExtendedAttributeField(attribute, template.getName());
			}
	}

	private void addExtendedAttributeField(GUIAttribute attribute, String templateName) {
		DataSourceField field = null;
		String fieldName = "_" + attribute.getName().replace(" ", Constants.BLANK_PLACEHOLDER);
		String fieldTitle = attribute.getLabel() + " (" + templateName + ")";
		if (attribute.getType() == GUIAttribute.TYPE_DATE) {
			field = new DataSourceDateTimeField();
			field.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN);
			fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + GUIAttribute.TYPE_DATE;
		} else if (attribute.getType() == GUIAttribute.TYPE_DOUBLE) {
			field = new DataSourceFloatField();
			field.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN, OperatorId.EQUALS,
					OperatorId.NOT_EQUAL);
			fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + GUIAttribute.TYPE_DOUBLE;
		} else if (attribute.getType() == GUIAttribute.TYPE_INT) {
			field = new DataSourceIntegerField();
			field.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN, OperatorId.EQUALS,
					OperatorId.NOT_EQUAL);
			fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + GUIAttribute.TYPE_INT;
		} else if (attribute.getType() == GUIAttribute.TYPE_BOOLEAN) {
			field = new DataSourceIntegerField();
			field.setValidOperators(OperatorId.EQUALS);
			fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + GUIAttribute.TYPE_BOOLEAN;
		} else if (attribute.getType() == GUIAttribute.TYPE_USER || attribute.getType() == GUIAttribute.TYPE_FOLDER) {
			field = new DataSourceIntegerField();
			field.setValidOperators(OperatorId.EQUALS, OperatorId.NOT_EQUAL, OperatorId.IS_NULL, OperatorId.NOT_NULL);
			fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + attribute.getType();
		} else {
			field = new DataSourceTextField();
			field.setValidOperators(OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS, OperatorId.EQUALS,
					OperatorId.NOT_EQUAL);
			if (attribute.getEditor() == GUIAttribute.EDITOR_DEFAULT)
				fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + GUIAttribute.TYPE_STRING;
			else
				fieldName = fieldName + Constants.BLANK_PLACEHOLDER + TYPE + GUIAttribute.TYPE_STRING_PRESET;
		}

		field.setName(fieldName);
		field.setTitle(fieldTitle);
		addField(field);
	}
}