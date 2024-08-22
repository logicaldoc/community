package com.logicaldoc.gui.frontend.client.menu;

import java.util.Arrays;
import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * A tray to interact with the Search
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class QuickSearchTray extends MenuTray {

	private static final String SEARCH = "search";

	private static final String FULLTEXT = "fulltext";

	private TextItem searchBox = new TextItem();

	SelectItem searchType = new SelectItem();

	public QuickSearchTray() {
		FormItemIcon search = new FormItemIcon();
		search.setInline(true);
		search.setInlineIconAlign(Alignment.RIGHT);
		search.setText(AwesomeFactory.getIconHtml(SEARCH));
		search.addFormItemClickHandler(event -> onSearch());

		searchBox.setShowTitle(false);
		searchBox.setDefaultValue(I18N.message(SEARCH) + "...");
		searchBox.setWidth(180);
		searchBox.setIcons(search);
		searchBox.addKeyPressHandler(event -> {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equalsIgnoreCase(event.getKeyName())) {
				onSearch();
			}
		});
		searchBox.addClickHandler(event -> {
			if ((I18N.message(SEARCH) + "...").equals(event.getItem().getValue())) {
				event.getItem().setValue("");
			}
		});

		LinkedHashMap<String, String> valueMap = new LinkedHashMap<>();
		valueMap.put(FULLTEXT, I18N.message(FULLTEXT));
		valueMap.put("filename", I18N.message("filename"));
		valueMap.put("id", I18N.message("id"));
		valueMap.put("customId", I18N.message("customid"));

		searchType.setWidth(130);
		searchType.setShowTitle(false);
		searchType.setValueMap(valueMap);
		searchType.setValue(FULLTEXT);

		if (Feature.enabled(Feature.PARAMETRIC_SEARCHES))
			setItems(searchBox, searchType);
		else
			setItems(searchBox);
	}

	private void onSearch() {
		GUISearchOptions options = new GUISearchOptions();

		Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(Session.get().getUser().getHitsGrid());
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("search.hits");
		options.setMaxHits(pageSize);

		String field = searchType.getValueAsString();
		String value = searchBox.getValueAsString().trim();
		if (FULLTEXT.equals(field)) {
			options.setType(GUISearchOptions.TYPE_FULLTEXT);
			options.setExpression(value);
			options.setExpressionLanguage(I18N.getLocale());
			options.setType(GUISearchOptions.TYPE_FULLTEXT);
			options.setFields(Constants.getFulltextDefaultFields());
			options.setCriteria(null);
		} else {
			options.setType(GUISearchOptions.TYPE_PARAMETRIC);
			options.setTopOperator("matchall");
			GUICriterion criterion = new GUICriterion();
			criterion.setField(field);
			criterion.setOperator("contains");
			if ("id".equals(field)) {
				criterion.setOperator("equals");
				try {
					criterion.setLongValue(Long.parseLong(value));
				} catch (Exception t) {
					criterion.setLongValue(0L);
				}
			} else
				criterion.setStringValue(value);
			options.setCaseSensitive(0);
			options.setCriteria(Arrays.asList(criterion));
		}

		Search.get().setOptions(options);
		Search.get().search();
	}
	
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof QuickSearchTray)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}