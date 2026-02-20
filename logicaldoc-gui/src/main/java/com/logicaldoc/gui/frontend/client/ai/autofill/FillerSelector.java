package com.logicaldoc.gui.frontend.client.ai.autofill;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.widgets.form.fields.SelectItem;


/**
 * List box to select a filler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
public class FillerSelector extends SelectItem {

	/**
	 * Creates a select list with the fillers
	 * 
	 * @param includeEmpty id an empty row must be shown
	 * @param fillerId identifier of the filler to be selected by default
	 */
	public FillerSelector(boolean includeEmpty, Long fillerId) {
		super("filler", I18N.message("filler"));
		setDisplayField("label");
		setValueField("id");
		setWidth(150);
		setMultiple(false);
		setWrapTitle(false);
		setMultipleAppearance(MultipleAppearance.PICKLIST);
		setOptionDataSource(new FillersDS(includeEmpty));

		if (fillerId != null)
			setValue(fillerId);
	}
}
