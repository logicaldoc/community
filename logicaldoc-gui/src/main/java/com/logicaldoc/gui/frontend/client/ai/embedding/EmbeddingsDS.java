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

		DataSourceTextField docId = new DataSourceTextField("docId");
		docId.setPrimaryKey(true);
		docId.setHidden(true);
		docId.setRequired(true);

		DataSourceTextField folderId = new DataSourceTextField("folderId");
		folderId.setPrimaryKey(true);
		folderId.setHidden(true);
		folderId.setRequired(true);

		DataSourceTextField fileName = new DataSourceTextField("filename");

		DataSourceTextField fileVersion = new DataSourceTextField("fileVersion");

		DataSourceTextField icon = new DataSourceTextField("icon");
		icon.setHidden(true);

		DataSourceFloatField fileSize = new DataSourceFloatField("size");
		DataSourceDateTimeField date = new DataSourceDateTimeField("date");

		setFields(id, docId, folderId, icon, fileName, fileVersion, fileSize, date);
		setClientOnly(true);

		setDataURL("data/ai.xml?object=embedding&schemeId=" + schemeId + "&max=" + (max != null ? max : DEFAULT_MAX));
	}
}