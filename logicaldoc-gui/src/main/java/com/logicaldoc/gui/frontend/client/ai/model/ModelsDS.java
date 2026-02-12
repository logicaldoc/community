package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.List;
import java.util.stream.Collectors;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle grids of ai models. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelsDS extends DataSource {

	public ModelsDS() {
		this((List<String>) null);
	}

	public ModelsDS(String type) {
		this(List.of(type));
	}

	public ModelsDS(List<String> types) {
		setRecordXPath("/list/model");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField name = new DataSourceTextField("name");
		DataSourceTextField label = new DataSourceTextField("label");
		DataSourceTextField description = new DataSourceTextField("description");
		DataSourceTextField typeField = new DataSourceTextField("type");
		DataSourceDateTimeField trained = new DataSourceDateTimeField("trained");
		DataSourceBooleanField training = new DataSourceBooleanField("training");
		DataSourceDateTimeField evaluated = new DataSourceDateTimeField("evaluated");
		DataSourceBooleanField evaluation = new DataSourceBooleanField("evaluation");

		setFields(id, name, label, training, trained, description, typeField, evaluated, evaluation);
		setClientOnly(true);

		setDataURL("data/ai.xml?object=model"
				+ (types != null && !types.isEmpty() ? "&type=" + types.stream().collect(Collectors.joining(","))
						: ""));
	}
}