package com.logicaldoc.gui.frontend.client.search;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.MiniDateRangeItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.PickerIcon;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a full-text search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FulltextForm extends VLayout implements SearchObserver {
	private static final String SEARCHINHITS = "searchinhits";

	private static final String CREATION_DATE_RANGE = "creationDateRange";

	private static final String PUBLICATION_DATE_RANGE = "publicationDateRange";

	private static final String LESSTHAN = "lessthan";

	private static final String NO_LANGUAGE = "";

	private ValuesManager vm = new ValuesManager();

	private DynamicForm fieldsForm = new DynamicForm();

	private MultiComboBoxItem searchinItem = null;

	private FolderSelector folder;

	public FulltextForm() {
		setHeight100();
		setOverflow(Overflow.AUTO);
		setMembersMargin(3);
		setAlign(Alignment.LEFT);
		Search.get().addObserver(this);
	}

	private void initGUI() {
		vm.clearValues();
		if(getMembers()!=null)
			removeMembers(getMembers());
				
		DynamicForm form1 = new DynamicForm();
		form1.setValuesManager(vm);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setNumCols(3);

		PickerIcon searchPicker = new PickerIcon(PickerIcon.SEARCH, new FormItemClickHandler() {
			public void onFormItemClick(FormItemIconClickEvent event) {
				search();
			}
		});
		PickerIcon clear = new PickerIcon(PickerIcon.CLEAR, new FormItemClickHandler() {
			public void onFormItemClick(FormItemIconClickEvent event) {
				initGUI();
			}
		});

		TextItem expression = ItemFactory.newTextItem("expression", "expression", I18N.message("search") + "...");
		expression.setWidth("*");
		expression.setColSpan(3);
		expression.setRequired(true);
		expression.setIcons(searchPicker, clear);
		expression.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() == null)
					return;
				if (Constants.KEY_ENTER.equals(event.getKeyName().toLowerCase()))
					search();
			}
		});
		expression.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if ((I18N.message("search") + "...").equals(event.getItem().getValue())) {
					event.getItem().setValue("");
				}
			}
		});

		CheckboxItem searchinhits = new CheckboxItem("searchinhits", I18N.message("searchinhits"));
		searchinhits.setColSpan(3);

		SelectItem language = ItemFactory.newLanguageSelector("language", true, false);
		language.setDefaultValue(NO_LANGUAGE);
		language.setColSpan(3);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);
		template.setMultiple(false);
		template.setColSpan(3);
		template.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue() != null && !"".equals((String) event.getValue()))
					prepareFields(Long.parseLong((String) event.getValue()));
				else
					prepareFields(null);
			}
		});

		CheckboxItem subfolders = new CheckboxItem("subfolders", I18N.message("searchinsubfolders"));

		CheckboxItem aliases = new CheckboxItem("aliases", I18N.message("retrievealiases"));
		aliases.setValue(true);
		aliases.setEndRow(true);

		folder = new FolderSelector(null, true);
		folder.setColSpan(3);
		folder.setWidth(200);

		if (Search.get().getOptions().getFolder() != null)
			folder.setFolder(Search.get().getOptions().getFolder(), Search.get().getOptions().getFolderName());

		SelectItem sizeOperator = ItemFactory.newSizeOperator("sizeOperator", "size");
		IntegerItem size = ItemFactory.newIntegerItem("size", " ", null);
		size.setWidth(50);
		size.setShowTitle(false);
		size.setHint("KB");
		size.setEndRow(true);

		MiniDateRangeItem creationDateRange = ItemFactory.newMiniDateRangeItem(CREATION_DATE_RANGE,
				I18N.message("created"));
		creationDateRange.setEndRow(true);
		creationDateRange.setColSpan(3);

		MiniDateRangeItem publicationDateRange = ItemFactory.newMiniDateRangeItem(PUBLICATION_DATE_RANGE,
				I18N.message("published"));
		publicationDateRange.setEndRow(true);
		publicationDateRange.setColSpan(3);

		form1.setItems(expression, language, searchinhits, folder, subfolders, aliases, sizeOperator, size,
				creationDateRange, publicationDateRange, template);

		addMember(form1);

		prepareFields(null);
	}

	@SuppressWarnings("rawtypes")
	private void search() {
		if (!vm.validate()) {
			return;
		}

		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();

		GUISearchOptions options = new GUISearchOptions();

		options.setMaxHits(Search.get().getMaxHits());
		options.setType(GUISearchOptions.TYPE_FULLTEXT);
		options.setExpression(vm.getValueAsString("expression"));
		if (NO_LANGUAGE.equals(vm.getValueAsString("language")) || vm.getValue("language") == null) {
			options.setLanguage(null);
			options.setExpressionLanguage(I18N.getLocale());
		} else {
			options.setLanguage(vm.getValueAsString("language"));
			options.setExpressionLanguage(options.getLanguage());
		}

		Long size = vm.getValueAsString("size") != null ? Long.parseLong(vm.getValueAsString("size")) : null;
		if (size != null) {
			if (LESSTHAN.equals(vm.getValueAsString("sizeOperator")))
				options.setSizeMax(size * 1024);
			else
				options.setSizeMin(size * 1024);
		}

		/*
		 * Check the creation date
		 */
		Map range = (Map) values.get(CREATION_DATE_RANGE);
		if (range != null) {
			Date start = (Date) range.get("start");
			if (start != null)
				options.setCreationFrom(start);

			Date end = (Date) range.get("end");
			if (end != null)
				options.setCreationTo(end);
		}

		/*
		 * Check the publication date
		 */
		range = (Map) values.get(PUBLICATION_DATE_RANGE);
		if (range != null) {
			Date start = (Date) range.get("start");
			if (start != null)
				options.setDateFrom(start);

			Date end = (Date) range.get("end");
			if (end != null)
				options.setDateTo(end);
		}

		try {
			if (values.containsKey("template") && values.get("template") != null
					&& !((String) values.get("template")).isEmpty())
				options.setTemplate(Long.parseLong((String) values.get("template")));
		} catch (Throwable t) {
		}

		String[] fields = searchinItem.getValues();
		options.setFields(fields);

		options.setFolder(folder.getFolderId());
		options.setFolderName(folder.getFolderName());

		options.setRetrieveAliases(Boolean.parseBoolean(vm.getValueAsString("aliases")) ? 1 : 0);

		options.setSearchInSubPath(Boolean.parseBoolean(vm.getValueAsString("subfolders")));
		if (Boolean.parseBoolean(vm.getValueAsString(SEARCHINHITS))) {
			GUIDocument[] docs = Search.get().getLastResult();
			Long[] ids = new Long[docs.length];
			int i = 0;
			for (GUIDocument doc : docs) {
				ids[i] = doc.getId();
				i++;
			}
			options.setFilterIds(ids);
		} else
			options.setFilterIds(null);

		options.setType(0);
		Search.get().setOptions(options);
		Search.get().search();
	}

	/*
	 * Prepare the form for the selectable fields
	 */
	private void prepareFields(Long templateId) {
		if (fieldsForm != null && contains(fieldsForm))
			removeMember(fieldsForm);

		fieldsForm = new DynamicForm();
		fieldsForm.setTitleOrientation(TitleOrientation.TOP);
		fieldsForm.setWidth100();
		addMember(fieldsForm);

		searchinItem = ItemFactory.newMultiComboBoxItem("searchin", "searchin", null, null);
		searchinItem.setWidth(300);

		final LinkedHashMap<String, String> fieldsMap = new LinkedHashMap<String, String>();
		fieldsMap.put(Constants.FULLTEXT_FIELD_FILENAME, I18N.message("filename"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_CONTENT, I18N.message("content"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_TAGS, I18N.message("tags"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_CUSTOMID, I18N.message("customid"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_COMMENT, I18N.message("comment"));
		fieldsMap.put(Constants.FULLTEXT_FIELD_NOTES, I18N.message("notes"));

		searchinItem.setValueMap(fieldsMap);
		searchinItem.setValue(Constants.FULLTEXT_DEFAULT_FIELDS);

		fieldsForm.setItems(searchinItem);

		if (templateId == null)
			return;

		DocumentService.Instance.get().getAttributes(templateId, new AsyncCallback<GUIAttribute[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttribute[] result) {
				for (GUIAttribute att : result) {
					if (att.getType() == GUIAttribute.TYPE_STRING && !att.isHidden()) {
						fieldsMap.put("ext_" + att.getName(), att.getDisplayName());
					}
				}
				searchinItem.setValueMap(fieldsMap);
			}
		});
	}

	@Override
	public void onSearchArrived() {

	}

	@Override
	public void onOptionsChanged(GUISearchOptions newOptions) {
		if (newOptions.getType() == GUISearchOptions.TYPE_FULLTEXT) {
			vm.setValue("expression", newOptions.getExpression());
			folder.setFolder(newOptions.getFolder(), newOptions.getFolderName());
		}
	}

	@Override
	protected void onDraw() {
		initGUI();
	}
}