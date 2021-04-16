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
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This class represents a Criterion Row for the Parametric Search.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class ParameterConditionRow extends HLayout {

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
		if (removeImg != null)
			removeMember(removeImg);
		if (form != null) {
			attribute.clearValue();
			operator.clearValue();
			value.clearValue();
			removeMember(form);
		}

		removeImg = new ImgButton();
		removeImg.setShowDown(false);
		removeImg.setShowRollOver(false);
		removeImg.setLayoutAlign(Alignment.LEFT);
		removeImg.setSrc("[SKIN]/headerIcons/close.gif");
		removeImg.setHeight(18);
		removeImg.setWidth(18);
		removeImg.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				ParameterConditionRow.this.getParentCanvas().removeChild(ParameterConditionRow.this);
			}
		});

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(4);
		form.setWidth(300);
		form.setHeight(20);

		attribute = new SelectItem("fields", "fields");
		attribute.setShowTitle(false);
		attribute.setPickListWidth(120);
		attribute.setWidth(120);
		attribute.setMultiple(false);
		DataSource ds = null;

		if (forDocument)
			ds = new DocumentFieldsDS(template);
		else
			ds = new FolderFieldsDS(template);

		LinkedHashMap<String, String> fieldsMap = new LinkedHashMap<String, String>();
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

		try {
			if (fieldSelected != null && !fieldSelected.trim().isEmpty())
				operatorsMap = operatorsFor(fieldSelected);
			else
				operatorsMap = operatorsFor(null);
		} catch (Throwable t) {
			SC.warn(t.getMessage());
		}

		operator.setValueMap(operatorsMap);
		if (!operatorsMap.isEmpty())
			operator.setValue(operatorsMap.keySet().iterator().next());

		attribute.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue() != null) {
					fieldSelected = (String) event.getValue();
					reload();
				}
			}
		});

		if (fieldSelected != null && !fieldSelected.trim().isEmpty()) {
			value = valueItemFor(fieldSelected);
		} else {
			value = ItemFactory.newTextItem("value", "value", null);
		}
		value.setRequired(true);
		value.setEndRow(true);
		value.setShowTitle(false);
		value.setColSpan(1);

		if (valueKeyPressHandler != null)
			value.addKeyPressHandler(valueKeyPressHandler);

		form.setItems(attribute, operator, value);

		setMembers(removeImg, form);

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				if (value instanceof DateItem)
					return;

				int padSize = ParameterConditionRow.this.getWidth() - 230;
				if (padSize < 100)
					padSize = 100;
				value.setWidth(padSize);
			}
		});

		operator.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onOperatorChanged(event != null ? event.getValue().toString() : null);
			}
		});

		onOperatorChanged(operator.getValue() != null ? operator.getValue().toString() : null);
	}

	private void onOperatorChanged(String valueOperator) {
		if (valueOperator == null || "null".equals(valueOperator) || "notnull".equals(valueOperator)) {
			form.hideItem("value");
			if (value != null)
				value.setVisible(false);
		} else {
			form.showItem("value");
			if (value != null)
				value.setVisible(true);
		}
	}

	private LinkedHashMap<String, String> operatorsFor(String criteriaField) {
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		if (criteriaField == null)
			return map;

		if (criteriaField.equals("id") || criteriaField.equals("fileSize") || criteriaField.equals("rating")
				|| criteriaField.equals("published") || criteriaField.equals("indexed")
				|| criteriaField.endsWith("type:" + GUIAttribute.TYPE_INT)
				|| criteriaField.endsWith("type:" + GUIAttribute.TYPE_DOUBLE)) {
			map.put("greaterthan", I18N.message("greaterthan").toLowerCase());
			map.put("lessthan", I18N.message("lessthan").toLowerCase());
			map.put("equals", I18N.message("equals").toLowerCase());
			map.put("notequal", I18N.message("notequal").toLowerCase());
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		} else if (criteriaField.equals("sourceDate") || criteriaField.equals("lastModified")
				|| criteriaField.equals("date") || criteriaField.equals("creation")
				|| criteriaField.equals("startPublishing") || criteriaField.equals("stopPublishing")
				|| criteriaField.endsWith("type:" + GUIAttribute.TYPE_DATE)) {
			map.put("greaterthan", I18N.message("greaterthan").toLowerCase());
			map.put("lessthan", I18N.message("lessthan").toLowerCase());
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_BOOLEAN)) {
			map.put("equals", I18N.message("equals").toLowerCase());
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_STRING_PRESET)
				|| criteriaField.endsWith("type:" + GUIAttribute.TYPE_USER)
				|| criteriaField.endsWith("type:" + GUIAttribute.TYPE_FOLDER)) {
			map.put("equals", I18N.message("equals").toLowerCase());
			map.put("notequal", I18N.message("notequal").toLowerCase());
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		} else if (criteriaField.equals("tags") || criteriaField.equals("notes")) {
			map.put("contains", I18N.message("contains").toLowerCase());
			map.put("notcontains", I18N.message("notcontains").toLowerCase());
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		} else if (criteriaField.equals("template")) {
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		} else {
			map.put("contains", I18N.message("contains").toLowerCase());
			map.put("notcontains", I18N.message("notcontains").toLowerCase());
			map.put("equals", I18N.message("equals").toLowerCase());
			map.put("notequal", I18N.message("notequal").toLowerCase());
			map.put("beginswith", I18N.message("beginswith").toLowerCase());
			map.put("endswith", I18N.message("endswith").toLowerCase());
			map.put("null", I18N.message("isnull").toLowerCase());
			map.put("notnull", I18N.message("isnotnull").toLowerCase());
		}

		return map;
	}

	private FormItem valueItemFor(String criteriaField) {
		if (criteriaField.equals("id") || criteriaField.equals("fileSize") || criteriaField.equals("rating")
				|| criteriaField.equals("template") || criteriaField.equals("published")
				|| criteriaField.equals("indexed") || criteriaField.endsWith("type:" + GUIAttribute.TYPE_INT)) {
			return ItemFactory.newIntegerItem("value", "integer", null);
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_DOUBLE)) {
			return ItemFactory.newFloatItem("value", "double", null);
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_BOOLEAN)) {
			FormItem item = ItemFactory.newBooleanSelector("value", "boolean");
			item.setValue("yes");
			return item;
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_STRING_PRESET)) {
			String attributeName = criteriaField.substring(0, criteriaField.lastIndexOf(':') - 4).replaceAll("_", "");
			FormItem item = ItemFactory.newStringItemForAttribute(template.getAttribute(attributeName));
			item.setName("value");
			return item;
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_USER)) {
			String attributeName = criteriaField.substring(0, criteriaField.lastIndexOf(':') - 4).replaceAll("_", "");
			GUIAttribute att = template.getAttribute(attributeName);
			FormItem item = ItemFactory.newUserSelectorForAttribute("value", att.getLabel(),
					(att.getOptions() != null && att.getOptions().length > 0) ? att.getOptions()[0] : null, null);
			item.setName("value");
			return item;
		} else if (criteriaField.endsWith("type:" + GUIAttribute.TYPE_FOLDER)) {
			String attributeName = criteriaField.substring(0, criteriaField.lastIndexOf(':') - 4).replaceAll("_", "");
			GUIAttribute att = template.getAttribute(attributeName);
			FormItem item = ItemFactory.newFolderSelectorForAttribute("value", att.getLabel(), false, null);
			item.setName("value");
			return item;
		} else if (criteriaField.equals("sourceDate") || criteriaField.equals("lastModified")
				|| criteriaField.equals("date") || criteriaField.equals("creation")
				|| criteriaField.equals("startPublishing") || criteriaField.equals("stopPublishing")
				|| criteriaField.endsWith("type:" + GUIAttribute.TYPE_DATE)) {
			return ItemFactory.newDateItem("value", "date");
		} else if (criteriaField.equals("tags")) {
			return ItemFactory.newTagsMultiplePickList("value", "tags", new TagsDS(null, false, null, null), null);
		} else {
			return ItemFactory.newTextItem("value", "text", null);
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