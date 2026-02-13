package com.logicaldoc.gui.frontend.client.ai.model;

import java.util.ArrayList;
import java.util.List;

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
		this(type != null ? createList(type) : null);
	}

	private static List<String> createList(String type) {
		List<String> list = new ArrayList<>();
		list.add(type);
		return list;
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

		String url = "data/ai.xml?object=model";

		if (types != null && !types.isEmpty()) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < types.size(); i++) {
				if (i > 0)
					sb.append(",");
				sb.append(types.get(i));
			}
			url += "&type=" + sb.toString();
		}

		setDataURL(url);
	}
}
