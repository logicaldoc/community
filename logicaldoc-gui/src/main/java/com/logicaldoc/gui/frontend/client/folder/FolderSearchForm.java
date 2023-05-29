package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.UserSelector;
import com.logicaldoc.gui.frontend.client.search.ParameterConditionRow;
import com.logicaldoc.gui.frontend.client.search.Search;
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
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a folders search form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4.2
 */
public abstract class FolderSearchForm extends VLayout {

	private static final String TEMPLATE = "template";

	private static final String TYPE = "type:";

	private static final String CASESENSITIVE = "casesensitive";

	private ValuesManager vm = new ValuesManager();

	private FolderSelector folderSelector;

	private GUITemplate selectedTemplate = null;

	private VLayout conditionsLayout = null;

	protected FolderSearchForm() {
		setHeight100();
		setWidth100();
		setMargin(3);
		setMembersMargin(3);
		setAlign(Alignment.LEFT);
		setOverflow(Overflow.AUTO);

		addResizedHandler(event -> {
			if (conditionsLayout.getMembers() != null)
				for (Canvas row : conditionsLayout.getMembers()) {
					row.setWidth(FolderSearchForm.this.getWidth() - 10);
				}
		});
	}

	private void initGUI() {
		vm.clearValues();
		if (getMembers() != null)
			removeMembers(getMembers());

		CheckboxItem subfolders = new CheckboxItem("subfolders", I18N.message("searchinsubfolders2"));
		subfolders.setEndRow(true);

		folderSelector = new FolderSelector(null, null);
		folderSelector.setTitle(I18N.message("parent"));
		folderSelector.setEndRow(true);
		folderSelector.setWidth(160);
		folderSelector.addFolderChangeListener(folder -> {
			if (folder != null)
				subfolders.setValue(true);
			else
				subfolders.setValue(false);
		});

		final DynamicForm folderForm = new DynamicForm();
		folderForm.setValuesManager(vm);
		folderForm.setTitleOrientation(TitleOrientation.TOP);

		folderForm.setItems(folderSelector, subfolders);

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
		topLayout.setMembers(folderForm, spacer, search, reset);
		addMember(topLayout);
		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		CheckboxItem casesensitive = new CheckboxItem(CASESENSITIVE, I18N.message(CASESENSITIVE));
		casesensitive.setValue(true);
		CheckboxItem aliases = new CheckboxItem("aliases", I18N.message("retrievealiases"));

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
				if (event.getValue() != null && !"".equals((String) event.getValue())) {
					TemplateService.Instance.get().getTemplate(Long.parseLong((String) event.getValue()),
							new AsyncCallback<GUITemplate>() {
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

			form.setItems(casesensitive, aliases, template, match);
		} else {
			form.setItems(casesensitive, aliases, match);
		}
		addMember(form);

		IButton add = new IButton(I18N.message("addcondition"));
		add.setAutoFit(true);
		add.addClickHandler(event -> addCondition());
		addMember(add);

		conditionsLayout = new VLayout(3);
		addMember(conditionsLayout);
		addNameCondition();
	}

	public void removeCondition(ParameterConditionRow criteria) {
		conditionsLayout.removeMember(criteria);
	}

	class SearchOnEnter implements KeyPressHandler {
		@Override
		public void onKeyPress(KeyPressEvent event) {
			if (event.getKeyName() == null)
				return;
			if (Constants.KEY_ENTER.equals(event.getKeyName().toLowerCase()))
				search();
		}

	}

	/**
	 * Add the old-style condition on the folder's name
	 */
	public void addNameCondition() {
		ParameterConditionRow row = new ParameterConditionRow(null, false, new SearchOnEnter());
		row.setAttribute("name");
		row.setWidth(getWidth() - 10);
		row.reload();
		conditionsLayout.addMember(row);
	}

	public void addCondition() {
		ParameterConditionRow row = new ParameterConditionRow(selectedTemplate, false, new SearchOnEnter());
		row.setWidth(getWidth() - 10);
		row.reload();
		conditionsLayout.addMember(row);
	}

	private void search() {
		search(prepareOptions());
	}

	/**
	 * Prepare the search options.
	 */
	protected GUISearchOptions prepareOptions() {
		if (Boolean.FALSE.equals(vm.validate()))
			return null;

		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();

		GUISearchOptions options = new GUISearchOptions();
		options.setMaxHits(Search.get().getMaxHits());
		options.setRetrieveAliases(Boolean.parseBoolean(vm.getValueAsString("aliases")) ? 1 : 0);
		options.setCaseSensitive(Boolean.parseBoolean(vm.getValueAsString(CASESENSITIVE)) ? 1 : 0);

		options.setType(GUISearchOptions.TYPE_FOLDERS);

		setTemplateOption(values, options);

		options.setTopOperator((String) values.get("match"));

		options.setSearchInSubPath(Boolean.parseBoolean(vm.getValueAsString("subfolders")));

		List<GUICriterion> criteria = new ArrayList<>();
		if (conditionsLayout.getMembers() != null)
			for (Canvas canvas : conditionsLayout.getMembers()) {
				ParameterConditionRow condition = (ParameterConditionRow) canvas;
				addCriterion(condition, criteria);
			}

		addFolderCriterion(options, criteria);

		options.setCriteria(criteria.toArray(new GUICriterion[0]));

		return options;
	}

	private void addCriterion(ParameterConditionRow condition, List<GUICriterion> criteria) {
		GUICriterion criterion = new GUICriterion();

		String fieldName = condition.getAttributeFieldItem().getValueAsString();
		fieldName = fieldName.replace(Constants.BLANK_PLACEHOLDER, " ");

		String fieldOperator = condition.getOperatorsFieldItem().getValueAsString();
		Object fieldValue = getFieldValue(condition);

		if (fieldName.startsWith("_"))
			fieldName = fieldName.substring(1);
		if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_INT) || fieldName.endsWith(TYPE + GUIAttribute.TYPE_USER)
				|| fieldName.endsWith(TYPE + GUIAttribute.TYPE_FOLDER))
			fieldValue = Long.parseLong(fieldValue.toString());
		else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_DOUBLE))
			fieldValue = Double.parseDouble(fieldValue.toString());
		else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_BOOLEAN))
			fieldValue = fieldValue.toString().equals("yes") ? 1L : 0L;
		else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_DATE))
			fieldValue = (Date) fieldValue;
		else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_STRING_PRESET)) {
			fieldName = fieldName.replace(TYPE + GUIAttribute.TYPE_STRING_PRESET, TYPE + GUIAttribute.TYPE_STRING);
		} else if (fieldName.endsWith(TYPE + GUIAttribute.TYPE_STRING_TEXTAREA)) {
			fieldName = fieldName.replace(TYPE + GUIAttribute.TYPE_STRING_TEXTAREA, TYPE + GUIAttribute.TYPE_STRING);
		}
		criterion.setField(fieldName);

		setCriterionValue(criterion, fieldValue);

		criterion.setOperator(fieldOperator.toLowerCase());

		addCriterion(condition, criteria, criterion, fieldOperator);
	}

	private void setCriterionValue(GUICriterion criterion, Object fieldValue) {
		if (fieldValue instanceof Date)
			criterion.setDateValue((Date) fieldValue);
		else if (fieldValue instanceof Integer)
			criterion.setLongValue(((Integer) fieldValue).longValue());
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

	private Object getFieldValue(ParameterConditionRow condition) {
		Object fieldValue = condition.getValueFieldItem().getValue();

		// This lines are necessary to avoid error for GWT values type.
		if (condition.getValueFieldItem() instanceof IntegerItem)
			fieldValue = Long.parseLong(fieldValue.toString());
		if (condition.getValueFieldItem() instanceof UserSelector)
			fieldValue = ((UserSelector) condition.getValueFieldItem()).getUser().getId();
		if (condition.getValueFieldItem() instanceof FolderSelector)
			fieldValue = ((FolderSelector) condition.getValueFieldItem()).getFolder().getId();
		return fieldValue;
	}

	private void addCriterion(ParameterConditionRow conditionRow, List<GUICriterion> criteria, GUICriterion criterion,
			String fieldOperator) {
		if (!criterion.getField().equals("tags")) {
			criteria.add(criterion);
		} else {
			// In case of tags, we will have to create a criterion per
			// tag
			if (criterion.getField().equals("tags")) {
				String[] tgs = ((SelectItem) conditionRow.getValueFieldItem()).getValues();
				for (String tag : tgs) {
					GUICriterion c = new GUICriterion();
					c.setField(criterion.getField());
					c.setOperator(fieldOperator.toLowerCase());
					c.setStringValue(tag);
					criteria.add(c);
				}
			}
		}
	}

	private void addFolderCriterion(GUISearchOptions options, List<GUICriterion> criteria) {
		if (folderSelector != null) {
			options.setFolder(folderSelector.getFolderId());
			options.setFolderName(folderSelector.getFolderName());

			GUICriterion criterion = new GUICriterion();
			criterion.setField("folder");
			criterion.setOperator("in");
			criterion.setLongValue(options.getFolder());
			if (options.isSearchInSubPath())
				criterion.setOperator("inorsubfolders");
			criteria.add(criterion);
		}
	}

	private void setTemplateOption(Map<String, Object> values, GUISearchOptions options) {
		if (values.containsKey(TEMPLATE) && values.get(TEMPLATE) != null && !((String) values.get(TEMPLATE)).isEmpty())
			options.setTemplate(Long.parseLong((String) values.get(TEMPLATE)));
	}

	/**
	 * Implementations must put here the search logic
	 * 
	 * @param options The filter options
	 */
	abstract protected void search(GUISearchOptions options);

	@Override
	protected void onDraw() {
		initGUI();
	}
}