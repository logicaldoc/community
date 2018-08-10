package com.logicaldoc.gui.frontend.client.menu;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;

/**
 * This panel shows the quick search controls
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchBox extends TextItem {

	public SearchBox() {
		PickerIcon searchPicker = new PickerIcon(PickerIcon.SEARCH, new FormItemClickHandler() {
			public void onFormItemClick(FormItemIconClickEvent event) {
				onSearch();
			}
		});

		setShowTitle(false);
		setDefaultValue(I18N.message("search") + "...");
		setWidth(200);
		setIcons(searchPicker);
		addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() == null)
					return;
				if (Constants.KEY_ENTER.equals(event.getKeyName().toLowerCase())) {
					onSearch();
				}
			}
		});
		addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if ((I18N.message("search") + "...").equals(event.getItem().getValue())) {
					event.getItem().setValue("");
				}
			}
		});
	}

	protected void onSearch() {
		GUISearchOptions options = Search.get().getOptions();
		options.setType(GUISearchOptions.TYPE_FULLTEXT);
		options.setExpression((String) getValue());
		options.setExpressionLanguage(I18N.getLocale());
		options.setFolder(null);
		options.setTemplate(null);
		options.setLanguage(null);
		options.setFilterIds(null);
		options.setFields(Constants.FULLTEXT_DEFAULT_FIELDS);

		String hits = Session.get().getConfig("search.hits");
		options.setMaxHits(Integer.parseInt(hits));
		Search.get().setOptions(options);
		Search.get().search();
	}
}