package com.logicaldoc.gui.common.client.data;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceTextField;

/**
 * Datasource to retrieve all the attributes available for the documents
 * declared in the attribute sets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6 n
 */
public class AttributesDS extends DataSource {

    public AttributesDS(boolean sections) {
        this(null, null, sections, false);
    }

    public AttributesDS(String context) {
        this(null, context, false, false);
    }

    public AttributesDS(String context, boolean sections) {
        this(null, context, sections, false);
    }

    public AttributesDS(String context, boolean sections, boolean extOnly) {
        this(null, context, sections, extOnly);
    }

    public AttributesDS(Long templateId) {
        this(templateId, null, false, false);
    }

    public AttributesDS(Long templateId, String context, boolean sections, boolean extOnly) {
        setTitleField("attributes");
        setRecordXPath("/list/attribute");
        DataSourceTextField name = new DataSourceTextField("name");
        name.setPrimaryKey(true);

        DataSourceTextField label = new DataSourceTextField("label");

        DataSourceTextField type = new DataSourceTextField("type");
        type.setHidden(true);

        setFields(name, label, type);
        setTitleField("name");
        setDataURL("data/attributes.xml?locale=" + I18N.getLocale()
                + (templateId != null ? "&templateId=" + templateId : "")
                + (context != null ? "&context=" + context : "") + "&sections=" + sections + "&extOnly=" + extOnly);
        setClientOnly(true);
    }
}