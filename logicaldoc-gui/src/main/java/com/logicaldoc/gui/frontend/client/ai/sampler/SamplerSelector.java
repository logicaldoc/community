package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A drop-dopwn list to select a sampler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerSelector extends SelectItem {

	private static final String NAME = "name";

	public SamplerSelector() {
		setName("sampler");
		setTitle(I18N.message("sampler"));
		setMultiple(false);

		setOptionDataSource(new SamplerDS(null));
		setValueField("id");
		setDisplayField(NAME);
		setSortField(NAME);

		ListGridField id = new IdListGridField();
		ListGridField name = new ListGridField(NAME, I18N.message(NAME));
		ListGridField type = new ListGridField("type", I18N.message("type"));
		setPickListWidth(200);
		setPickListFields(id, name, type);
	}

	public GUISampler getSelectedSampler() {
		ListGridRecord selection = getSelectedRecord();
		if (selection == null)
			return null;

		GUISampler sampler = new GUISampler();
		sampler.setId(selection.getAttributeAsLong("id"));
		sampler.setName(selection.getAttributeAsString(NAME));
		sampler.setType(selection.getAttributeAsString("type"));
		return sampler;
	}
}
