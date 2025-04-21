package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Data source to show notes in the posts portlet. It is based on Xml parsing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EventsDS extends DataSource {
	public EventsDS(EventsDSParameter parameter) {
		setRecordXPath("/list/event");
		DataSourceTextField code = new DataSourceTextField("code");
		code.setPrimaryKey(true);
		code.setHidden(true);
		code.setRequired(true);
		DataSourceTextField type = new DataSourceTextField("type");
		DataSourceTextField label = new DataSourceTextField("label");

		setFields(code, type, label);
		setClientOnly(true);

		setDataURL("data/events.xml?locale=" + I18N.getLocale() + "&folder=" + parameter.isFolder() + "&workflow=" + parameter.isWorkflow()
				+ "&user=" + parameter.isUser() + "&importfolder=" + parameter.isImportfolder() + "&ocr=" + parameter.isOcr() + "&webservice=" + parameter.isWebservice()+ "&ai=" + parameter.isAi()
				+ "&all=" + parameter.isAll());
	}
}