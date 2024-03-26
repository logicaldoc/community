package com.logicaldoc.gui.frontend.client.metadata.form;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.FieldType;

/**
 * Data Source to handle the responses of a form. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ResponsesDS extends DataSource {

	public static final Integer DEFAULT_MAX = 100;

	/**
	 * Constructor.
	 * 
	 * @param form The form
	 * @param max maximum number of elements
	 */
	public ResponsesDS(GUIForm form, Integer max) {
		prepareFields(form);
		setDataURL("data/formresponses.xml?locale=" + Session.get().getUser().getLanguage() + "&max="
				+ (max != null ? max : DEFAULT_MAX) + "&formId=" + form.getId());
	}

	private void prepareFields(GUIForm form) {
		setTitleField("filename");
		setRecordXPath("/list/document");
		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField respondent = new DataSourceTextField("_respondent");

		DataSourceTextField filename = new DataSourceTextField("filename");

		DataSourceDateTimeField created = new DataSourceDateTimeField("created");
		DataSourceTextField template = new DataSourceTextField("template");
		template.setHidden(true);

		DataSourceTextField language = new DataSourceTextField("language");

		List<DataSourceField> fields = new ArrayList<>();
		fields.add(id);
		fields.add(filename);
		fields.add(created);
		fields.add(respondent);

		fields.add(template);
		fields.add(language);

		for (String name : form.getAttributeNames()) {
			DataSourceTextField ext = new DataSourceTextField("ext_" + name, name);
			ext.setHidden(true);
			ext.setCanFilter(true);
			if (form.getAttribute(name).getType() == GUIAttribute.TYPE_SECTION)
				continue;

			GUIAttribute attDef = Session.get().getInfo().getAttributeDefinition(name);

			if (attDef != null) {
				if (attDef.getType() == GUIAttribute.TYPE_DATE) {
					ext.setType(FieldType.DATE);
					ext.setCanFilter(false);
				} else if (attDef.getType() == GUIAttribute.TYPE_INT) {
					ext.setType(FieldType.INTEGER);
					ext.setCanFilter(false);
				} else if (attDef.getType() == GUIAttribute.TYPE_DOUBLE) {
					ext.setType(FieldType.FLOAT);
					ext.setCanFilter(false);
				} else if (attDef.getType() == GUIAttribute.TYPE_SECTION) {
					continue;
				}
			}

			fields.add(ext);
		}

		setFields(fields.toArray(new DataSourceField[0]));
		setClientOnly(true);
	}
}