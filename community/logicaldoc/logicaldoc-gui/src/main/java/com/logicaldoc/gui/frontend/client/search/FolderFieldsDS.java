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

		setFields(id, folderName, description, created, creator, tags);

		/*
		 * Define extended attributes
		 */
		if (template != null && template.getAttributes() != null)
			for (GUIAttribute att : template.getAttributes()) {
				DataSourceField field = null;
				String name = "_" + att.getName().replaceAll(" ", Constants.BLANK_PLACEHOLDER);
				String titl = att.getLabel() + " (" + template.getName() + ")";
				if (att.getType() == GUIAttribute.TYPE_DATE) {
					field = new DataSourceDateTimeField();
					field.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN);
					name = name + Constants.BLANK_PLACEHOLDER + "type:" + GUIAttribute.TYPE_DATE;
				} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
					field = new DataSourceFloatField();
					field.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN, OperatorId.EQUALS,
							OperatorId.NOT_EQUAL);
					name = name + Constants.BLANK_PLACEHOLDER + "type:" + GUIAttribute.TYPE_DOUBLE;
				} else if (att.getType() == GUIAttribute.TYPE_INT) {
					field = new DataSourceIntegerField();
					field.setValidOperators(OperatorId.GREATER_THAN, OperatorId.LESS_THAN, OperatorId.EQUALS,
							OperatorId.NOT_EQUAL);
					name = name + Constants.BLANK_PLACEHOLDER + "type:" + GUIAttribute.TYPE_INT;
				} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
					field = new DataSourceIntegerField();
					field.setValidOperators(OperatorId.EQUALS);
					name = name + Constants.BLANK_PLACEHOLDER + "type:" + GUIAttribute.TYPE_BOOLEAN;
				} else {
					field = new DataSourceTextField();
					field.setValidOperators(OperatorId.ICONTAINS, OperatorId.INOT_CONTAINS, OperatorId.EQUALS,
							OperatorId.NOT_EQUAL);
					if (att.getEditor() == GUIAttribute.EDITOR_DEFAULT)
						name = name + Constants.BLANK_PLACEHOLDER + "type:" + GUIAttribute.TYPE_STRING;
					else
						name = name + Constants.BLANK_PLACEHOLDER + "type:" + GUIAttribute.TYPE_STRING_PRESET;
				}

				field.setName(name);
				field.setTitle(titl);
				addField(field);
			}
	}
}