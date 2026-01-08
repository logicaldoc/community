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
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.embedding.EmbeddingSchemesDS;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.search.ParametricForm;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.search.SearchPanel;
import com.logicaldoc.gui.frontend.client.search.SemanticForm;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.OperatorId;
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

	private static final String SEMANTIC = "semantic";

	private TextItem searchBox = new TextItem();

	private SelectItem searchType = new SelectItem("type");
	
	private SelectItem embeddingScheme = ItemFactory.newSelectItem("scheme");

	public QuickSearchTray() {
		setNumCols(3);

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

		if (Feature.enabled(Feature.PARAMETRIC_SEARCHES)) {
			valueMap.put("filename", I18N.message("filename"));
			valueMap.put("id", I18N.message("id"));
			valueMap.put("customId", I18N.message("customid"));
		}

		if (Feature.enabled(Feature.SEMANTIC_SEARCHES))
			valueMap.put(SEMANTIC, I18N.message(SEMANTIC));

		searchType.setWidth(130);
		searchType.setShowTitle(false);
		searchType.setValueMap(valueMap);
		searchType.setValue(FULLTEXT);

		embeddingScheme.setOptionDataSource(new EmbeddingSchemesDS(true));
		embeddingScheme.setValueField("id");
		embeddingScheme.setDisplayField("label");
		embeddingScheme.setRequired(true);
		embeddingScheme.setDefaultToFirstOption(true);
		embeddingScheme.setShowTitle(false);
		embeddingScheme.setWidth(150);
		embeddingScheme.setVisibleWhen(new AdvancedCriteria("type", OperatorId.EQUALS, SEMANTIC));

		if (Feature.enabled(Feature.PARAMETRIC_SEARCHES) || Feature.enabled(Feature.SEMANTIC_SEARCHES))
			setItems(searchBox, embeddingScheme, searchType);
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
			options.setFields(Constants.getFulltextDefaultFields());
			options.setCriteria(null);
		} else if (SEMANTIC.equals(field)) {
			options.setType(GUISearchOptions.TYPE_SEMANTIC);
			options.setExpression(value);
			options.setExpressionLanguage(I18N.getLocale());
			options.setThreshold(50);
			options.setEmbeddingSchemeId(embeddingScheme.getValueAsLong());
			
			// Just to make sure the form gets initialized and registered as listener
			SemanticForm.get();
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
			options.setCaseSensitive(false);
			options.setCriteria(Arrays.asList(criterion));
			
			// Just to make sure the form gets initialized and registered as listener
			ParametricForm.get();
		}

		SearchPanel.get().onDraw();
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