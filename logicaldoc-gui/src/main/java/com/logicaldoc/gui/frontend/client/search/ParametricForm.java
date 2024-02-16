package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.DocumentSelector;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.UserSelector;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a parametric search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class ParametricForm extends VLayout {
	private static final String TEMPLATE = "template";

	private static final String TYPE = "type:";

	private static final String CASESENSITIVE = "casesensitive";

	private static final String LANGUAGE = "language";

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
		setOverflow(Overflow.AUTO);
		setMembersMargin(3);
		setAlign(Alignment.LEFT);

		addResizedHandler(event -> {
			if (conditionsLayout.getMembers() != null)
				for (Canvas row : conditionsLayout.getMembers()) {
					row.setWidth(ParametricForm.this.getWidth() - 10);
				}
		});
	}

	private void initGUI() {
		vm.clearValues();
		if (getMembers() != null)
			removeMembers(getMembers());

		final DynamicForm languageForm = new DynamicForm();
		languageForm.setValuesManager(vm);
		languageForm.setTitleOrientation(TitleOrientation.TOP);
		languageForm.setNumCols(1);
		SelectItem language = ItemFactory.newLanguageSelector(LANGUAGE, true, false);
		language.setDefaultValue("");

		languageForm.setItems(language);

		IButton search = new IButton(I18N.message("search"));
		search.setAutoFit(true);
		search.setMargin(8);
		search.addClickHandler(event -> search());

		IButton reset = new IButton(I18N.message("reset"));
		reset.setMargin(8);
		reset.setAutoFit(true);
		reset.addClickHandler(event -> initGUI());

		HLayout spacer = new HLayout();
		spacer.setMinWidth(30);

		HLayout topLayout = new HLayout();
		topLayout.setHeight(15);
		topLayout.setWidth(1);
		topLayout.setMembersMargin(3);
		topLayout.setAlign(VerticalAlignment.CENTER);
		topLayout.setMembers(languageForm, spacer, search, reset);
		addMember(topLayout);

		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		folder = new FolderSelector(null, null);
		folder.setColSpan(2);
		folder.setEndRow(true);
		folder.setWidth(200);

		CheckboxItem casesensitive = new CheckboxItem(CASESENSITIVE, I18N.message(CASESENSITIVE));
		casesensitive.setValue(true);
		CheckboxItem aliases = new CheckboxItem("aliases", I18N.message("retrievealiases"));
		aliases.setValue(true);

		CheckboxItem subfolders = new CheckboxItem("subfolders", I18N.message("searchinsubfolders2"));
		subfolders.setColSpan(2);
		subfolders.setEndRow(true);

		CheckboxItem searchinhits = new CheckboxItem(SEARCHINHITS, I18N.message(SEARCHINHITS));

		LinkedHashMap<String, String> matchMap = new LinkedHashMap<>();
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
			template.addChangedHandler(event -> {
				if (event.getValue() != null && !"".equals(event.getValue())) {
					TemplateService.Instance.get().getTemplate(Long.parseLong((String) event.getValue()),
							new AsyncCallback<>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUITemplate result) {
									selectedTemplate = result;
								}
							});
				} else {
					selectedTemplate = null;
				}
			});

			form.setItems(folder, subfolders, casesensitive, aliases, template, searchinhits, match);
		} else {
			form.setItems(folder, subfolders, casesensitive, aliases, searchinhits, match);
		}

		addMember(form);

		IButton add = new IButton(I18N.message("addcondition"));
		add.setAutoFit(true);
		add.addClickHandler(event -> addCondition());
		addMember(add);

		conditionsLayout = new VLayout(3);
		addMember(conditionsLayout);
	}

	public void removeCondition(ParameterConditionRow condition) {
		conditionsLayout.removeMember(condition);
	}

	public void addCondition() {
		ParameterConditionRow row = new ParameterConditionRow(selectedTemplate, true, event -> {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equalsIgnoreCase(event.getKeyName()))
				search();
		});
		row.setWidth(getWidth() - 10);
		row.reload();
		conditionsLayout.addMember(row);
	}

	@SuppressWarnings("unchecked")
	private void search() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		Map<String, Object> values = vm.getValues();

		GUISearchOptions options = new GUISearchOptions();

		options.setMaxHits(Search.get().getMaxHits());
		options.setRetrieveAliases(Boolean.parseBoolean(vm.getValueAsString("aliases")) ? 1 : 0);
		options.setCaseSensitive(Boolean.parseBoolean(vm.getValueAsString(CASESENSITIVE)) ? 1 : 0);

		options.setType(GUISearchOptions.TYPE_PARAMETRIC);

		setLanguageCondition(options);

		setTemplateCondition(values, options);

		options.setTopOperator((String) values.get("match"));

		setFolderCondition(options);

		List<GUICriterion> criteria = new ArrayList<>();
		if (conditionsLayout.getMembers() != null)
			addCriteria(criteria);

		// Handle language condition as additional criterion
		if (!NO_LANGUAGE.equals(vm.getValueAsString(LANGUAGE).trim())) {
			GUICriterion criterion = new GUICriterion();
			criterion.setField(LANGUAGE);
			criterion.setOperator("equals");
			criterion.setStringValue(vm.getValueAsString(LANGUAGE));
			criteria.add(criterion);
		}

		// Handle folder condition as additional criterion
		if (options.getFolder() != null) {
			GUICriterion criterion = new GUICriterion();
			criterion.setField("folder");
			criterion.setOperator("in");
			criterion.setLongValue(options.getFolder());
			if (options.isSearchInSubPath())
				criterion.setOperator("inorsubfolders");
			criteria.add(criterion);
		}

		options.setCriteria(criteria);

		addSearchInHitsCondition(options);

		Search.get().setOptions(options);
		Search.get().search();
	}

	private void addCriteria(List<GUICriterion> criteria) {
		for (Canvas canvas : conditionsLayout.getMembers()) {
			ParameterConditionRow condition = (ParameterConditionRow) canvas;
			String fieldName = condition.getAttributeFieldItem().getValueAsString();
			if (fieldName == null || fieldName.isEmpty())
				continue;

			addCriterion(condition, criteria);
		}
	}

	private void addCriterion(ParameterConditionRow condition, List<GUICriterion> criteria) {
		GUICriterion criterion = prepareCriterion(condition);

		if (!criterion.getField().equals("tags")) {
			criteria.add(criterion);
		} else {
			addTagsCriteria(condition, criteria, criterion.getField(), criterion.getOperator());
		}
	}

	private GUICriterion prepareCriterion(ParameterConditionRow condition) {
		String fieldOperator = condition.getOperatorsFieldItem().getValueAsString().toLowerCase();
		Object fieldValue = condition.getValueFieldItem() != null ? condition.getValueFieldItem().getValue() : null;

		GUICriterion criterion = new GUICriterion();
		criterion.setField(getFieldName(condition));
		if (fieldValue != null)
			setCriterionValueAndField(condition, fieldValue, criterion);

		criterion.setOperator(fieldOperator.toLowerCase());
		return criterion;
	}

	private void setCriterionValueAndField(ParameterConditionRow condition, Object fieldValue, GUICriterion criterion) {
		// This lines are necessary to avoid error for GWT values
		// type.
		if (condition.getValueFieldItem() instanceof IntegerItem)
			fieldValue = Long.parseLong(fieldValue.toString());
		if (condition.getValueFieldItem() instanceof UserSelector)
			fieldValue = ((UserSelector) condition.getValueFieldItem()).getUser().getId();
		if (condition.getValueFieldItem() instanceof FolderSelector)
			fieldValue = ((FolderSelector) condition.getValueFieldItem()).getFolder().getId();
		if (condition.getValueFieldItem() instanceof DocumentSelector)
			fieldValue = ((DocumentSelector) condition.getValueFieldItem()).getDocument().getId();

		String fieldName = criterion.getField();

		if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_INT) || fieldName.endsWith(TYPE + GUIAttribute.TYPE_USER)
				|| fieldName.endsWith(TYPE + GUIAttribute.TYPE_FOLDER)
				|| fieldName.endsWith(TYPE + GUIAttribute.TYPE_DOCUMENT)) {
			fieldValue = Long.parseLong(fieldValue.toString());
		} else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_DOUBLE)) {
			fieldValue = Double.parseDouble(fieldValue.toString());
		} else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_BOOLEAN)) {
			fieldValue = fieldValue.toString().equals("yes") ? 1L : 0L;
		}

		if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_STRING_PRESET)) {
			criterion.setField(
					fieldName.replace(TYPE + GUIAttribute.TYPE_STRING_PRESET, TYPE + GUIAttribute.TYPE_STRING));
		} else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_STRING_TEXTAREA)) {
			criterion.setField(
					fieldName.replace(TYPE + GUIAttribute.TYPE_STRING_TEXTAREA, TYPE + GUIAttribute.TYPE_STRING));
		}

		setCriterionValue(fieldValue, criterion);
	}

	private void setCriterionValue(Object fieldValue, GUICriterion criterion) {
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
			JSOHelper.convertToMap((JavaScriptObject) fieldValue);
		}
	}

	private String getFieldName(ParameterConditionRow condition) {
		String fieldName = condition.getAttributeFieldItem().getValueAsString();
		fieldName = fieldName.replace(Constants.BLANK_PLACEHOLDER, " ");
		if (fieldName.startsWith("_"))
			fieldName = fieldName.substring(1);
		return fieldName;
	}

	private void addTagsCriteria(ParameterConditionRow condition, List<GUICriterion> criteria, String fieldName,
			String fieldOperator) {
		// In case of tags, we will have to create a criterion per
		// tag
		if (fieldName.equals("tags")) {
			String[] tgs = ((SelectItem) condition.getValueFieldItem()).getValues();
			for (String tag : tgs) {
				GUICriterion c = new GUICriterion();
				c.setField(fieldName);
				c.setOperator(fieldOperator);
				c.setStringValue(tag);
				criteria.add(c);
			}
		}
	}

	private void addSearchInHitsCondition(GUISearchOptions options) {
		if (Boolean.parseBoolean(vm.getValueAsString(SEARCHINHITS))) {
			options.setFilterIds(
					Search.get().getLastResult().stream().map(rec -> rec.getId()).collect(Collectors.toList()));
		} else
			options.setFilterIds(new ArrayList<>());
	}

	private void setFolderCondition(GUISearchOptions options) {
		if (folder != null) {
			options.setFolder(folder.getFolderId());
			options.setFolderName(folder.getFolderName());
		}
		options.setSearchInSubPath(Boolean.parseBoolean(vm.getValueAsString("subfolders")));
	}

	private void setTemplateCondition(Map<String, Object> values, GUISearchOptions options) {
		if (values.containsKey(TEMPLATE) && values.get(TEMPLATE) != null && !((String) values.get(TEMPLATE)).isEmpty())
			options.setTemplate(Long.parseLong((String) values.get(TEMPLATE)));
	}

	private void setLanguageCondition(GUISearchOptions options) {
		if (NO_LANGUAGE.equals(vm.getValueAsString(LANGUAGE)))
			options.setLanguage(null);
		else
			options.setLanguage(vm.getValueAsString(LANGUAGE));
		options.setExpressionLanguage(I18N.getLocale());
	}

	@Override
	protected void onDraw() {
		initGUI();
	}
}