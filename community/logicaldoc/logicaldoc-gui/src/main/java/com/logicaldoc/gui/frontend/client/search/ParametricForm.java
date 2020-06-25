package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.UserSelector;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a parametric search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ParametricForm extends VLayout {
	private static final String SEARCHINHITS = "searchinhits";

	private static final String NO_LANGUAGE = "";

	private ValuesManager vm = new ValuesManager();

	private FolderSelector folder;

	private GUITemplate selectedTemplate = null;

	private VLayout conditionsLayout = null;

	private static ParametricForm instance;

	public static ParametricForm get() {
		if (instance == null)
			instance = new ParametricForm();
		return instance;
	}

	private ParametricForm() {
		setHeight100();
		setWidth100();
		setMargin(3);
		setMembersMargin(3);
		setAlign(Alignment.LEFT);

		setOverflow(Overflow.AUTO);

		final DynamicForm languageForm = new DynamicForm();
		languageForm.setValuesManager(vm);
		languageForm.setTitleOrientation(TitleOrientation.TOP);
		languageForm.setNumCols(2);
		SelectItem language = ItemFactory.newLanguageSelector("language", true, false);
		language.setDefaultValue("");
		languageForm.setItems(language);

		IButton search = new IButton(I18N.message("search"));
		search.setAutoFit(true);
		search.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				search();
			}
		});

		HLayout topLayout = new HLayout(80);
		topLayout.setMembers(languageForm, search);
		topLayout.setTop(3);
		topLayout.setHeight(15);
		addMember(topLayout);

		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		folder = new FolderSelector(null, true);
		folder.setColSpan(2);
		folder.setEndRow(true);
		folder.setWidth(200);

		CheckboxItem casesensitive = new CheckboxItem("casesensitive", I18N.message("casesensitive"));
		casesensitive.setValue(true);
		CheckboxItem aliases = new CheckboxItem("aliases", I18N.message("retrievealiases"));
		aliases.setValue(true);

		CheckboxItem subfolders = new CheckboxItem("subfolders", I18N.message("searchinsubfolders2"));
		subfolders.setColSpan(2);
		subfolders.setEndRow(true);

		CheckboxItem searchinhits = new CheckboxItem(SEARCHINHITS, I18N.message(SEARCHINHITS));

		LinkedHashMap<String, String> matchMap = new LinkedHashMap<String, String>();
		matchMap.put("and", I18N.message("matchall"));
		matchMap.put("or", I18N.message("matchany"));
		matchMap.put("not", I18N.message("matchnone"));
		RadioGroupItem match = new RadioGroupItem("match");
		match.setDefaultValue("and");
		match.setShowTitle(false);
		match.setValueMap(matchMap);
		match.setVertical(false);
		match.setWrap(false);
		match.setWrapTitle(false);
		match.setColSpan(4);

		if (Feature.visible(Feature.TEMPLATE)) {
			SelectItem template = ItemFactory.newTemplateSelector(true, null);
			template.setMultiple(false);
			template.setEndRow(true);
			template.addChangedHandler(new ChangedHandler() {
				@Override
				public void onChanged(ChangedEvent event) {
					if (event.getValue() != null && !"".equals((String) event.getValue())) {
						TemplateService.Instance.get().getTemplate(Long.parseLong((String) event.getValue()),
								new AsyncCallback<GUITemplate>() {
									@Override
									public void onFailure(Throwable caught) {
										Log.serverError(caught);
									}

									@Override
									public void onSuccess(GUITemplate result) {
										selectedTemplate = result;
									}
								});
					} else {
						selectedTemplate = null;
					}
				}
			});

			form.setItems(folder, subfolders, casesensitive, aliases, template, searchinhits, match);
		} else {
			form.setItems(folder, subfolders, casesensitive, aliases, searchinhits, match);
		}

		addMember(form);

		IButton add = new IButton(I18N.message("addcondition"));
		add.setAutoFit(true);
		add.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				addCondition();
			}
		});
		addMember(add);

		conditionsLayout = new VLayout(3);
		addMember(conditionsLayout);

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				if (conditionsLayout.getMembers() != null)
					for (Canvas row : conditionsLayout.getMembers()) {
						row.setWidth(ParametricForm.this.getWidth() - 10);
					}
			}
		});
	}

	public void removeCondition(ParameterConditionRow condition) {
		conditionsLayout.removeMember(condition);
	}

	public void addCondition() {
		ParameterConditionRow row = new ParameterConditionRow(selectedTemplate, true, new KeyPressHandler() {

			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() == null)
					return;
				if (Constants.KEY_ENTER.equals(event.getKeyName().toLowerCase()))
					search();
			}
		});
		row.setWidth(getWidth() - 10);
		row.reload();
		conditionsLayout.addMember(row);
	}

	@SuppressWarnings("unchecked")
	private void search() {
		if (!vm.validate())
			return;

		Map<String, Object> values = vm.getValues();

		GUISearchOptions options = new GUISearchOptions();

		options.setMaxHits(Session.get().getConfigAsInt("search.hits"));
		options.setRetrieveAliases(Boolean.parseBoolean(vm.getValueAsString("aliases")) ? 1 : 0);
		options.setCaseSensitive(Boolean.parseBoolean(vm.getValueAsString("casesensitive")) ? 1 : 0);

		options.setType(GUISearchOptions.TYPE_PARAMETRIC);

		if (NO_LANGUAGE.equals(vm.getValueAsString("language")))
			options.setLanguage(null);
		else
			options.setLanguage(vm.getValueAsString("language"));
		options.setExpressionLanguage(I18N.getLocale());

		if (values.containsKey("template") && values.get("template") != null
				&& !((String) values.get("template")).isEmpty())
			options.setTemplate(Long.parseLong((String) values.get("template")));

		options.setTopOperator((String) values.get("match"));

		if (folder != null) {
			options.setFolder(folder.getFolderId());
			options.setFolderName(folder.getFolderName());
		}

		options.setSearchInSubPath(Boolean.parseBoolean(vm.getValueAsString("subfolders")));

		List<GUICriterion> list = new ArrayList<GUICriterion>();
		if (conditionsLayout.getMembers() != null)
			for (Canvas canvas : conditionsLayout.getMembers()) {
				ParameterConditionRow condition = (ParameterConditionRow) canvas;
				String fieldName = condition.getAttributeFieldItem().getValueAsString();
				if (fieldName == null || fieldName.isEmpty())
					continue;

				fieldName = fieldName.replaceAll(Constants.BLANK_PLACEHOLDER, " ");
				if (fieldName.startsWith("_"))
					fieldName = fieldName.substring(1);
				String fieldOperator = condition.getOperatorsFieldItem().getValueAsString();
				Object fieldValue = condition.getValueFieldItem() != null ? condition.getValueFieldItem().getValue()
						: null;

				GUICriterion criterion = new GUICriterion();

				if (fieldValue != null) {
					// This lines are necessary to avoid error for GWT values
					// type.
					if (condition.getValueFieldItem() instanceof IntegerItem)
						fieldValue = Long.parseLong(fieldValue.toString());
					if (condition.getValueFieldItem() instanceof UserSelector)
						fieldValue = ((UserSelector) condition.getValueFieldItem()).getUser().getId();
					if (condition.getValueFieldItem() instanceof FolderSelector)
						fieldValue = ((FolderSelector) condition.getValueFieldItem()).getFolder().getId();

					if (fieldName.endsWith("type:" + GUIAttribute.TYPE_INT)
							|| fieldName.endsWith("type:" + GUIAttribute.TYPE_USER)
							|| fieldName.endsWith("type:" + GUIAttribute.TYPE_FOLDER))
						fieldValue = Long.parseLong(fieldValue.toString());
					else if (fieldName.endsWith("type:" + GUIAttribute.TYPE_DOUBLE))
						fieldValue = Double.parseDouble(fieldValue.toString());
					else if (fieldName.endsWith("type:" + GUIAttribute.TYPE_BOOLEAN))
						fieldValue = fieldValue.toString().equals("yes") ? 1L : 0L;
					else if (fieldName.endsWith("type:" + GUIAttribute.TYPE_DATE))
						fieldValue = (Date) fieldValue;
					else if (fieldName.endsWith("type:" + GUIAttribute.TYPE_STRING_PRESET)) {
						fieldName = fieldName.replaceAll("type:" + GUIAttribute.TYPE_STRING_PRESET,
								"type:" + GUIAttribute.TYPE_STRING);
					} else if (fieldName.endsWith("type:" + GUIAttribute.TYPE_STRING_TEXTAREA)) {
						fieldName = fieldName.replaceAll("type:" + GUIAttribute.TYPE_STRING_TEXTAREA,
								"type:" + GUIAttribute.TYPE_STRING);
					}
					if (fieldValue instanceof Date)
						criterion.setDateValue((Date) fieldValue);
					else if (fieldValue instanceof Integer)
						criterion.setLongValue(Long.valueOf((Integer) fieldValue));
					else if (fieldValue instanceof Long)
						criterion.setLongValue((Long) fieldValue);
					else if (fieldValue instanceof Float)
						criterion.setDoubleValue(((Float) fieldValue).doubleValue());
					else if (fieldValue instanceof Double)
						criterion.setDoubleValue((Double) fieldValue);
					else if (fieldValue instanceof String)
						criterion.setStringValue((String) fieldValue);
					else if (fieldValue instanceof JavaScriptObject) {
						Map m = JSOHelper.convertToMap((JavaScriptObject) fieldValue);
					}
				}

				criterion.setField(fieldName);
				criterion.setOperator(fieldOperator.toLowerCase());

				if (!fieldName.equals("tags")) {
					list.add(criterion);
				} else {
					// In case of tags, we will have to create a criterion per
					// tag
					if (fieldName.equals("tags")) {
						String[] tgs = ((SelectItem) condition.getValueFieldItem()).getValues();
						for (String tag : tgs) {
							GUICriterion c = new GUICriterion();
							c.setField(fieldName);
							c.setOperator(fieldOperator.toLowerCase());
							c.setStringValue(tag);
							list.add(c);
						}
					}
				}
			}

		if (!NO_LANGUAGE.equals(vm.getValueAsString("language").trim())) {
			GUICriterion criterion = new GUICriterion();
			criterion.setField("language");
			criterion.setOperator("equals");
			criterion.setStringValue(vm.getValueAsString("language"));
			list.add(criterion);
		}

		if (options.getFolder() != null) {
			GUICriterion criterion = new GUICriterion();
			criterion.setField("folder");
			criterion.setOperator("in");
			criterion.setLongValue(options.getFolder());
			if (options.isSearchInSubPath())
				criterion.setOperator("inorsubfolders");
			list.add(criterion);
		}

		options.setCriteria(list.toArray(new GUICriterion[0]));

		if (Boolean.parseBoolean(vm.getValueAsString(SEARCHINHITS))) {
			GUIDocument[] records = Search.get().getLastResult();
			Long[] ids = new Long[records.length];
			int i = 0;
			for (GUIDocument rec : records) {
				ids[i] = rec.getId();
				i++;
			}
			options.setFilterIds(ids);
		} else
			options.setFilterIds(null);

		Search.get().setOptions(options);
		Search.get().search();
	}
}