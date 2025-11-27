package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
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

	private DynamicForm fieldsForm = new DynamicForm();

	private MultiComboBoxItem searchinItem = null;

	private static SemanticForm instance;

	private FolderSelector folder;

	private TextItem expression;

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

		DynamicForm form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setNumCols(3);

		FormItemIcon search = new FormItemIcon();
		search.setPrompt(I18N.message(SEARCH));
		search.setSrc("[SKIN]/icons/magnifying-glass.png");
		search.addFormItemClickHandler(click -> search());

		FormItemIcon clear = new FormItemIcon();
		clear.setPrompt(I18N.message("clear"));
		clear.setSrc("[SKIN]/icons/trash.png");
		clear.addFormItemClickHandler(click -> {
			vm.clearValues();
			prepareFieldsSelector(null);
		});

		expression = ItemFactory.newTextItem(EXPRESSION_STR, I18N.message(SEARCH) + "...");
		expression.setWidth("*");
		expression.setColSpan(3);
		expression.setRequired(true);
		expression.setIcons(search, clear);
		expression.addKeyPressHandler(event -> {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equalsIgnoreCase(event.getKeyName()))
				search();
		});
		expression.addClickHandler(event -> {
			if ((I18N.message(SEARCH) + "...").equals(event.getItem().getValue())) {
				event.getItem().setValue("");
			}
		});

		CheckboxItem searchinhits = new CheckboxItem(SEARCHINHITS, I18N.message(SEARCHINHITS));
		searchinhits.setColSpan(3);

		SelectItem language = ItemFactory.newLanguageSelector(LANGUAGE, true, false);
		language.setDefaultValue(NO_LANGUAGE);
		language.setColSpan(3);

		CheckboxItem subfolders = new CheckboxItem("subfolders", I18N.message("searchinsubfolders"));

		folder = new FolderSelector(null, null);
		folder.setColSpan(3);
		folder.setWidth(200);

		if (Search.get().getOptions().getFolder() != null)
			folder.setFolder(Search.get().getOptions().getFolder(), Search.get().getOptions().getFolderName());

		SelectItem sizeOperator = ItemFactory.newSizeOperator("sizeOperator", "size");
		sizeOperator.setTitleVAlign(VerticalAlignment.CENTER);
		IntegerItem size = ItemFactory.newIntegerItem("size", " ", (Integer) null);
		size.setWidth(50);
		size.setShowTitle(false);
		size.setHint("KB");
		size.setEndRow(true);

		form1.setItems(expression, language, searchinhits, folder, subfolders, sizeOperator, size);

		addMember(form1);

		prepareFieldsSelector(null);

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

		options.setFolder(folder.getFolderId());
		options.setFolderName(folder.getFolderName());

		options.setRetrieveAliases(Boolean.parseBoolean(vm.getValueAsString("aliases")));

		setSubfolderCondition(options);

//		options.setType(GUISearchOptions.TYPE_SEMANTIC);
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

	/**
	 * Prepare the form for the selectable fields
	 */
	private void prepareFieldsSelector(Long templateId) {
		if (templateId == null)
			prepareFieldsSelector(null, null);
		else
			TemplateService.Instance.get().getAttributes(templateId, null, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(List<GUIAttribute> templateAttributes) {
					prepareFieldsSelector(templateId, templateAttributes);
				}
			});
	}

	private void prepareFieldsSelector(Long templateId, List<GUIAttribute> templateAttributes) {
		if (fieldsForm != null && contains(fieldsForm))
			removeMember(fieldsForm);

		fieldsForm = new DynamicForm();
		fieldsForm.setTitleOrientation(TitleOrientation.TOP);
		fieldsForm.setWidth100();
		addMember(fieldsForm);

		searchinItem = ItemFactory.newMultiComboBoxItem("searchin", "searchin", null, null);
		searchinItem.setWidth(300);

		final LinkedHashMap<String, String> fieldsMap = new LinkedHashMap<>();
		fieldsMap.put(Constants.FULLTEXT_FIELD_FILENAME, I18N.message("filename"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_CONTENT, I18N.message("content"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_TAGS, I18N.message("tags"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_CUSTOMID, I18N.message("customid"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_COMMENT, I18N.message("comment"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_NOTES, I18N.message("notes"));

		if (templateId != null) {
			for (GUIAttribute att : templateAttributes.stream()
					.filter(att -> att.getType() == GUIAttribute.TYPE_STRING && !att.isHidden())
					.collect(Collectors.toList()))
				fieldsMap.put("ext_" + att.getName(), att.getDisplayName());
		}
		searchinItem.setValueMap(fieldsMap);

		if (defaultOptions != null)
			searchinItem.setValue(defaultOptions.getFields().toArray(new String[0]));
		else
			searchinItem.setValue(Constants.getFulltextDefaultFields());
		fieldsForm.setItems(searchinItem);
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