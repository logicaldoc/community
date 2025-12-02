package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.ai.embedding.EmbeddingSchemesDS;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a semantic search form
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class SemanticForm extends VLayout implements SearchObserver {
	private static final String SEARCH = "search";

	private static final String LANGUAGE = "language";

	private static final String EXPRESSION_STR = "expression";

	private static final String SEARCHINHITS = "searchinhits";

	private static final String LESSTHAN = "lessthan";

	private static final String NO_LANGUAGE = "";

	private ValuesManager vm = new ValuesManager();

	private MultiComboBoxItem searchinItem = null;

	private static SemanticForm instance;

	private FolderSelector folder;

	private TextAreaItem expression;

	private GUISearchOptions defaultOptions;

	public static SemanticForm get() {
		if (instance == null)
			instance = new SemanticForm();
		return instance;
	}

	public SemanticForm() {
		setHeight100();
		setOverflow(Overflow.AUTO);
		setMembersMargin(3);
		setAlign(Alignment.LEFT);
		Search.get().addObserver(this);
	}

	private void initGUI() {
		vm.clearValues();
		if (getMembers() != null)
			removeMembers(getMembers());

		FormItemIcon search = new FormItemIcon();
		search.setPrompt(I18N.message(SEARCH));
		search.setSrc("[SKIN]/icons/magnifying-glass.png");
		search.addFormItemClickHandler(click -> search());

		FormItemIcon clear = new FormItemIcon();
		clear.setPrompt(I18N.message("clear"));
		clear.setSrc("[SKIN]/icons/trash.png");
		clear.addFormItemClickHandler(click -> vm.clearValues());

		FormItemIcon expandIcon = new FormItemIcon();
		expandIcon.setSrc("[SKIN]/icons/pencil.png");
		expandIcon.setPrompt(I18N.message("freetextarea"));
		expandIcon.setWidth(12);
		expandIcon.setHeight(12);

		expandIcon.addFormItemClickHandler(event -> {
			TextAreaItem area = ItemFactory.newTextAreaItem("expressionEditor", null);
			area.setHeight(500);
			area.setWidth("100%");
			area.setValue(expression.getValue());

			LD.askForValue(I18N.message("freetextarea"), null, (String) expression.getValue(), area, 550,
					newValue -> expression.setValue(newValue));

			event.cancel();
		});

		expression = ItemFactory.newTextAreaItem(EXPRESSION_STR, "");
		expression.setHint(I18N.message(SEARCH) + "...");
		expression.setShowHintInField(true);
		expression.setWidth("*");
		expression.setHeight(50);
		expression.setColSpan(2);
		expression.setRequired(true);
		expression.setIcons(search, clear, expandIcon);

		expression.addKeyPressHandler(event -> {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equalsIgnoreCase(event.getKeyName()))
				search();
		});

		SelectItem language = ItemFactory.newLanguageSelector(LANGUAGE, true, false);
		language.setDefaultValue(NO_LANGUAGE);

		SelectItem embeddingScheme = ItemFactory.newSelectItem("embeddingscheme");
		embeddingScheme.setOptionDataSource(new EmbeddingSchemesDS(true));
		embeddingScheme.setValueField("id");
		embeddingScheme.setDisplayField("label");
		embeddingScheme.setRequired(true);
		embeddingScheme.setDefaultToFirstOption(true);

		folder = new FolderSelector(null, null);
		folder.setWidth(200);

		if (Search.get().getOptions().getFolder() != null)
			folder.setFolder(Search.get().getOptions().getFolder(), Search.get().getOptions().getFolderName());

		CheckboxItem subfolders = new CheckboxItem("subfolders", I18N.message("searchinsubfolders"));

		SpinnerItem threshold = new SpinnerItem("threshold", I18N.message("threshold"));
		threshold.setMin(0);
		threshold.setStep(1);
		threshold.setWidth(100);
		threshold.setMax(100);
		threshold.setDefaultValue(50);

		CheckboxItem searchinhits = new CheckboxItem(SEARCHINHITS, I18N.message(SEARCHINHITS));

		CheckboxItem aliases = new CheckboxItem("aliases", I18N.message("retrievealiases"));
		aliases.setValue(true);

		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		form.setItems(expression, language, embeddingScheme, folder, threshold, subfolders, searchinhits, aliases);

		addMember(form);

		applyOptions(defaultOptions);
	}

	private void search() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		defaultOptions = null;

		GUISearchOptions options = new GUISearchOptions();
		options.setSource(this);

		options.setMaxHits(Search.get().getMaxHits());
		options.setType(GUISearchOptions.TYPE_SEMANTIC);
		options.setExpression(vm.getValueAsString(EXPRESSION_STR));

		setLanguageCondition(options);
		setSizeCondition(options);

		List<String> fields = new ArrayList<>();
		Collections.addAll(fields, searchinItem.getValues());

		if (fields.contains(Constants.FULLTEXT_FIELD_FILENAME) && !fields.contains(Constants.FULLTEXT_FIELD_TITLE))
			fields.add(Constants.FULLTEXT_FIELD_TITLE);
		if (fields.contains(Constants.FULLTEXT_FIELD_TITLE) && !fields.contains(Constants.FULLTEXT_FIELD_FILENAME))
			fields.add(Constants.FULLTEXT_FIELD_FILENAME);

		options.setFields(fields);

		options.setEmbeddingSchemeId(Long.parseLong(vm.getValueAsString("embeddingscheme")));

		options.setFolder(folder.getFolderId());
		options.setFolderName(folder.getFolderName());

		options.setRetrieveAliases(Boolean.parseBoolean(vm.getValueAsString("aliases")));

		options.setThreshold(Integer.parseInt(vm.getValueAsString("threshold")));

		setSubfolderCondition(options);

		Search.get().setOptions(options);
		Search.get().search();
	}

	private void setSubfolderCondition(GUISearchOptions options) {
		options.setSearchInSubPath(Boolean.parseBoolean(vm.getValueAsString("subfolders")));
		if (Boolean.parseBoolean(vm.getValueAsString(SEARCHINHITS)))
			options.setFilterIds(
					Search.get().getLastResult().stream().map(doc -> doc.getId()).collect(Collectors.toList()));
		else
			options.setFilterIds(new ArrayList<>());
	}

	private void setSizeCondition(GUISearchOptions options) {
		Long size = vm.getValueAsString("size") != null ? Long.parseLong(vm.getValueAsString("size")) : null;
		if (size != null) {
			if (LESSTHAN.equals(vm.getValueAsString("sizeOperator")))
				options.setSizeMax(size * 1024);
			else
				options.setSizeMin(size * 1024);
		}
	}

	private void setLanguageCondition(GUISearchOptions options) {
		if (NO_LANGUAGE.equals(vm.getValueAsString(LANGUAGE)) || vm.getValue(LANGUAGE) == null) {
			options.setLanguage(null);
			options.setExpressionLanguage(I18N.getLocale());
		} else {
			options.setLanguage(vm.getValueAsString(LANGUAGE));
			options.setExpressionLanguage(options.getLanguage());
		}
	}

	@Override
	public void onSearchArrived() {
		// Nothing to do
	}

	@Override
	public void onOptionsChanged(GUISearchOptions options) {
		if (options.getType() == GUISearchOptions.TYPE_SEMANTIC) {
			defaultOptions = options;
			SearchMenu.get().openSemanticSection();
			if (isDrawn())
				applyOptions(options);
		}
	}

	private void applyOptions(GUISearchOptions options) {
		if (options == null)
			return;

		folder.setFolder(options.getFolder(), options.getFolderName());

		vm.setValue(EXPRESSION_STR, options.getExpression());
		vm.setValue("subfolders", options.isSearchInSubPath());
		vm.setValue("aliases", options.isRetrieveAliases());
		vm.setValue(LANGUAGE, options.getLanguage());
		if (options.getSizeMax() != null) {
			vm.setValue("size", options.getSizeMax() / 1024);
			vm.setValue("sizeOperator", LESSTHAN);
		}
		if (options.getSizeMin() != null) {
			vm.setValue("size", options.getSizeMin() / 1024);
			vm.setValue("sizeOperator", "greaterthan");
		}
		vm.setValue("threshold", options.getThreshold());
	}

	@Override
	protected void onDraw() {
		initGUI();
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}