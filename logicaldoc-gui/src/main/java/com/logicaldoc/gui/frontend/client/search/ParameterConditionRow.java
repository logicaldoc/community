package com.logicaldoc.gui.frontend.client.search;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.data.TagsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This class represents a Criterion Row for the Parametric Search.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class ParameterConditionRow extends HLayout {

	private static final String NOTCONTAINS = "notcontains";

	private static final String CONTAINS = "contains";

	private static final String ISNOTNULL = "isnotnull";

	private static final String ISNULL = "isnull";

	private static final String NOTEQUAL = "notequal";

	private static final String EQUALS = "equals";

	private static final String LESSTHAN = "lessthan";

	private static final String GREATERTHAN = "greaterthan";

	private static final String TYPE = "type:";

	private static final String NOTNULL = "notnull";

	private static final String VALUE_STR = "value";

	private ImgButton removeImg = null;

	private DynamicForm form = null;

	private SelectItem attribute = null;

	private SelectItem operator = null;

	private FormItem value = null;

	private GUITemplate template = null;

	private String fieldSelected = "";

	private boolean forDocument;

	private KeyPressHandler valueKeyPressHandler;

	public ParameterConditionRow(GUITemplate templ, boolean forDocument, KeyPressHandler valueKeyPressHandler) {
		setMembersMargin(5);
		setAlign(Alignment.LEFT);
		setHeight(5);

		this.template = templ;
		this.forDocument = forDocument;
		this.valueKeyPressHandler = valueKeyPressHandler;
		reload();
	}

	public void reload() {
		prepareForm();

		removeImg = new ImgButton();
		removeImg.setShowDown(false);
		removeImg.setShowRollOver(false);
		removeImg.setLayoutAlign(Alignment.LEFT);
		removeImg.setSrc("[SKIN]/headerIcons/close.gif");
		removeImg.setHeight(18);
		removeImg.setWidth(18);
		removeImg.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			ParameterConditionRow.this.getParentCanvas().removeChild(ParameterConditionRow.this);
		});

		attribute = new SelectItem("fields", "fields");
		attribute.setShowTitle(false);
		attribute.setPickListWidth(120);
		attribute.setWidth(120);
		attribute.setMultiple(false);

		DataSource ds = prepareDataSource();

		LinkedHashMap<String, String> fieldsMap = new LinkedHashMap<>();
		fieldsMap.put("", " ");
		for (DataSourceField sourceField : ds.getFields())
			fieldsMap.put(sourceField.getName(), I18N.message(sourceField.getTitle()));

		attribute.setValueMap(fieldsMap);
		attribute.setValue(fieldSelected);
		attribute.setColSpan(1);

		operator = new SelectItem("operators", "operators");
		operator.setPickListWidth(90);
		operator.setWidth(90);
		operator.setMultiple(false);
		operator.setShowTitle(false);
		operator.setColSpan(1);

		LinkedHashMap<String, String> operatorsMap = null;
		if (fieldSelected != null && !fieldSelected.trim().isEmpty())
			operatorsMap = operatorsFor(fieldSelected);
		else
			operatorsMap = operatorsFor(null);
		operator.setValueMap(operatorsMap);
		if (operatorsMap != null && !operatorsMap.isEmpty())
			operator.setValue(operatorsMap.keySet().iterator().next());

		attribute.addChangedHandler((ChangedEvent event) -> {
			if (event.getValue() != null) {
				fieldSelected = (String) event.getValue();
				reload();
			}
		});

		if (fieldSelected != null && !fieldSelected.trim().isEmpty()) {
			value = valueItemFor(fieldSelected);
		} else {
			value = ItemFactory.newTextItem(VALUE_STR, null);
		}
		value.setRequired(true);
		value.setEndRow(true);
		value.setShowTitle(false);
		value.setColSpan(1);

		if (valueKeyPressHandler != null)
			value.addKeyPressHandler(valueKeyPressHandler);

		form.setItems(attribute, operator, value);

		setMembers(removeImg, form);

		addResizeHandler();

		operator.addChangedHandler((ChangedEvent event) -> {
			onOperatorChanged(event != null ? event.getValue().toString() : null);
		});

		onOperatorChanged(operator.getValue() != null ? operator.getValue().toString() : null);
	}

	private void addResizeHandler() {
		addResizedHandler((ResizedEvent event) -> {
			if (value instanceof DateItem)
				return;

			int padSize = ParameterConditionRow.this.getWidth() - 230;
			if (padSize < 100)
				padSize = 100;
			value.setWidth(padSize);
		});
	}

	private DataSource prepareDataSource() {
		DataSource ds = null;
		if (forDocument)
			ds = new DocumentFieldsDS(template);
		else
			ds = new FolderFieldsDS(template);
		return ds;
	}

	private void prepareForm() {
		if (removeImg != null)
			removeMember(removeImg);
		if (form != null) {
			attribute.clearValue();
			operator.clearValue();
			value.clearValue();
			removeMember(form);
		}

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(4);
		form.setWidth(300);
		form.setHeight(20);
	}

	private void onOperatorChanged(String valueOperator) {
		if (valueOperator == null || "null".equals(valueOperator) || NOTNULL.equals(valueOperator)) {
			form.hideItem(VALUE_STR);
			if (value != null)
				value.setVisible(false);
		} else {
			form.showItem(VALUE_STR);
			if (value != null)
				value.setVisible(true);
		}
	}

	private LinkedHashMap<String, String> operatorsFor(String criteriaField) {
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		if (criteriaField == null)
			return map;

		if (criteriaField.equals("id") || criteriaField.equals("fileSize") || criteriaField.equals("pages")
				|| criteriaField.equals("rating") || criteriaField.equals("published")
				|| criteriaField.equals("indexed") || criteriaField.endsWith(TYPE + GUIAttribute.TYPE_INT)
				|| criteriaField.endsWith(TYPE + GUIAttribute.TYPE_DOUBLE)) {
			map.put(GREATERTHAN, I18N.message(GREATERTHAN).toLowerCase());
			map.put(LESSTHAN, I18N.message(LESSTHAN).toLowerCase());
			map.put(EQUALS, I18N.message(EQUALS).toLowerCase());
			map.put(NOTEQUAL, I18N.message(NOTEQUAL).toLowerCase());
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		} else if (criteriaField.equals("sourceDate") || criteriaField.equals("lastModified")
				|| criteriaField.equals("date") || criteriaField.equals("creation")
				|| criteriaField.equals("startPublishing") || criteriaField.equals("stopPublishing")
				|| criteriaField.endsWith(TYPE + GUIAttribute.TYPE_DATE)) {
			map.put(GREATERTHAN, I18N.message(GREATERTHAN).toLowerCase());
			map.put(LESSTHAN, I18N.message(LESSTHAN).toLowerCase());
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_BOOLEAN)) {
			map.put(EQUALS, I18N.message(EQUALS).toLowerCase());
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_STRING_PRESET)
				|| criteriaField.endsWith(TYPE + GUIAttribute.TYPE_USER)
				|| criteriaField.endsWith(TYPE + GUIAttribute.TYPE_FOLDER)) {
			map.put(EQUALS, I18N.message(EQUALS).toLowerCase());
			map.put(NOTEQUAL, I18N.message(NOTEQUAL).toLowerCase());
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		} else if (criteriaField.equals("tags") || criteriaField.equals("notes")) {
			map.put(CONTAINS, I18N.message(CONTAINS).toLowerCase());
			map.put(NOTCONTAINS, I18N.message(NOTCONTAINS).toLowerCase());
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		} else if (criteriaField.equals("template")) {
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		} else {
			map.put(CONTAINS, I18N.message(CONTAINS).toLowerCase());
			map.put(NOTCONTAINS, I18N.message(NOTCONTAINS).toLowerCase());
			map.put(EQUALS, I18N.message(EQUALS).toLowerCase());
			map.put(NOTEQUAL, I18N.message(NOTEQUAL).toLowerCase());
			map.put("beginswith", I18N.message("beginswith").toLowerCase());
			map.put("endswith", I18N.message("endswith").toLowerCase());
			map.put("null", I18N.message(ISNULL).toLowerCase());
			map.put(NOTNULL, I18N.message(ISNOTNULL).toLowerCase());
		}

		return map;
	}

	private FormItem valueItemFor(String criteriaField) {
		if (criteriaField.equals("id") || criteriaField.equals("fileSize") || criteriaField.equals("pages")
				|| criteriaField.equals("rating") || criteriaField.equals("template")
				|| criteriaField.equals("published") || criteriaField.equals("indexed")
				|| criteriaField.endsWith(TYPE + GUIAttribute.TYPE_INT)) {
			return ItemFactory.newIntegerItem(VALUE_STR, "integer", null);
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_DOUBLE)) {
			return ItemFactory.newFloatItem(VALUE_STR, "double", null);
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_BOOLEAN)) {
			FormItem item = ItemFactory.newBooleanSelector(VALUE_STR, "boolean");
			item.setValue("yes");
			return item;
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_STRING_PRESET)) {
			String attributeName = criteriaField.substring(0, criteriaField.lastIndexOf(':') - 4).replace("_", "");
			FormItem item = ItemFactory.newStringItemForAttribute(template.getAttribute(attributeName));
			item.setName(VALUE_STR);
			return item;
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_USER)) {
			String attributeName = criteriaField.substring(0, criteriaField.lastIndexOf(':') - 4).replace("_", "");
			GUIAttribute att = template.getAttribute(attributeName);
			FormItem item = ItemFactory.newUserSelectorForAttribute(VALUE_STR, att.getLabel(),
					(att.getOptions() != null && att.getOptions().length > 0) ? att.getOptions()[0] : null, null);
			item.setName(VALUE_STR);
			return item;
		} else if (criteriaField.endsWith(TYPE + GUIAttribute.TYPE_FOLDER)) {
			String attributeName = criteriaField.substring(0, criteriaField.lastIndexOf(':') - 4).replace("_", "");
			GUIAttribute att = template.getAttribute(attributeName);
			FormItem item = ItemFactory.newFolderSelectorForAttribute(VALUE_STR, att.getLabel(), false, null);
			item.setName(VALUE_STR);
			return item;
		} else if (criteriaField.equals("sourceDate") || criteriaField.equals("lastModified")
				|| criteriaField.equals("date") || criteriaField.equals("creation")
				|| criteriaField.equals("startPublishing") || criteriaField.equals("stopPublishing")
				|| criteriaField.endsWith(TYPE + GUIAttribute.TYPE_DATE)) {
			return ItemFactory.newDateItem(VALUE_STR, "date");
		} else if (criteriaField.equals("tags")) {
			return ItemFactory.newTagsMultiplePickList(VALUE_STR, "tags", new TagsDS(null, false, null, null), null);
		} else {
			return ItemFactory.newTextItem(VALUE_STR, null);
		}
	}

	public SelectItem getAttributeFieldItem() {
		return attribute;
	}

	public SelectItem getOperatorsFieldItem() {
		return operator;
	}

	public FormItem getValueFieldItem() {
		return value;
	}

	public void setAttribute(String attributeName) {
		attribute.setValue(attributeName);
		fieldSelected = attributeName;
		reload();
	}
}