package com.logicaldoc.gui.frontend.client.google.drive;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This panel shows the quick search controls
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchBox extends TextItem {

	public SearchBox() {
		setShowTitle(false);
		setDefaultValue(I18N.message("search") + "...");
		setWidth(160);

		FormItemIcon search = new FormItemIcon();
		search.setPrompt(I18N.message("search"));
		search.setSrc("[SKIN]/icons/magnifying-glass.png");
		search.addFormItemClickHandler(click -> onSearch());
		setIcons(search);
		
		addKeyPressHandler(event -> {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equalsIgnoreCase(event.getKeyName())) {
				onSearch();
			}
		});
		
		addClickHandler(event -> {
			if ((I18N.message("search") + "...").equals(event.getItem().getValue())) {
				event.getItem().setValue("");
			}
		});
	}

	protected void onSearch() {
		// Nothing to do
	}
}