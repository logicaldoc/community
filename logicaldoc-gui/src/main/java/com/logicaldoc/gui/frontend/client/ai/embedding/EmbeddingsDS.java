package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceDateTimeField;
import com.smartgwt.client.data.fields.DataSourceFloatField;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to handle grids of embeddings. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingsDS extends DataSource {

	private static final Integer DEFAULT_MAX = 100;

	public EmbeddingsDS(long schemeId, Integer max) {
		setRecordXPath("/list/embedding");

		DataSourceTextField id = new DataSourceTextField("id");
		id.setPrimaryKey(true);
		id.setHidden(true);
		id.setRequired(true);

		DataSourceTextField docid = new DataSourceTextField("docid");
		docid.setPrimaryKey(true);
		docid.setHidden(true);
		docid.setRequired(true);

		DataSourceTextField folderid = new DataSourceTextField("folderid");
		folderid.setPrimaryKey(true);
		folderid.setHidden(true);
		folderid.setRequired(true);

		DataSourceTextField filename = new DataSourceTextField("filename");

		DataSourceTextField icon = new DataSourceTextField("icon");
		icon.setHidden(true);

		DataSourceFloatField size = new DataSourceFloatField("size");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");

		setFields(id, docid, folderid, icon, filename, size, date);
		setClientOnly(true);

		setDataURL("data/ai.xml?object=embedding&schemeId=" + schemeId + "&max=" + (max != null ? max : DEFAULT_MAX));
	}
}