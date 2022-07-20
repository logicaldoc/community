package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.Date;

import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FloatItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A visual editor for initialization routines
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class AttributeInitializerComposer extends Window {

	private ValuesManager vm = new ValuesManager();

	private FormItem sourceItem = null;

	private int attributeType = GUIAttribute.TYPE_STRING;

	public AttributeInitializerComposer(FormItem sourceItem, int attributeType) {
		super();

		this.sourceItem = sourceItem;
		this.attributeType = attributeType;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("initializercomposer"));
		setAutoSize(true);

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		initGUI(attributeType);
	}

	private void initGUI(int attributeType) {
		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (sourceItem != null && vm.validate()) {
					sourceItem.clearErrors();
					sourceItem.setValue(composeAutomation());
					destroy();
				}
			}
		});
		save.setDisabled(sourceItem == null || sourceItem.isDisabled());

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				destroy();
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);
		toolStrip.addFill();

		DynamicForm wizardForm = null;
		if (attributeType == GUIAttribute.TYPE_DOUBLE)
			wizardForm = prepareFloatForm();
		else if (attributeType == GUIAttribute.TYPE_INT)
			wizardForm = prepareIntegerForm();
		else if (attributeType == GUIAttribute.TYPE_DATE)
			wizardForm = prepareDatesForm();
		else if (attributeType == GUIAttribute.TYPE_STRING || attributeType == GUIAttribute.TYPE_STRING_PRESET
				|| attributeType == GUIAttribute.TYPE_STRING_TEXTAREA)
			wizardForm = prepareTextForm();
		addItem(wizardForm);
		addItem(toolStrip);
	}

	protected String composeAutomation() {
		StringBuffer sb = new StringBuffer();

		if (attributeType == GUIAttribute.TYPE_DATE) {
			Date date = (Date) vm.getValue("date");
			boolean currentdate = vm.getValue("currentdate") != null ? (Boolean) vm.getValue("currentdate") : false;
			if (currentdate) {
				sb.append("$attribute.setDateValue($CURRENT_DATE);");
			} else {
				String dateStr = date != null ? DateUtil.format(date, "yyyy-MM-dd") : null;
				sb.append("#set($dt = $DateTool.parse('" + dateStr + "', 'yyyy-MM-dd'))\n");
				sb.append("$attribute.setDateValue($dt);");
			}
		} else if (attributeType == GUIAttribute.TYPE_INT) {
			Long number = Long.valueOf(vm.getValueAsString("number"));
			sb.append("#set($number = " + number + ")\n");
			sb.append("$attribute.setIntValue($number);");

		} else if (attributeType == GUIAttribute.TYPE_DOUBLE) {
			Double number = Double.valueOf(vm.getValueAsString("number"));
			sb.append("#set($number = " + number + ")\n");
			sb.append("$attribute.setDoubleValue($number);");
		} else if (attributeType == GUIAttribute.TYPE_STRING || attributeType == GUIAttribute.TYPE_STRING_PRESET
				|| attributeType == GUIAttribute.TYPE_STRING_TEXTAREA) {
			String text = vm.getValueAsString("value");
			sb.append("$attribute.setStringValue('" + text + "');");
		}

		return sb.toString();
	}

	private DynamicForm prepareFloatForm() {
		FloatItem number = ItemFactory.newFloatItem("number", "number", null);
		number.setWrapTitle(false);
		number.setRequired(true);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), number, new SpacerItem());

		return form;
	}

	private DynamicForm prepareIntegerForm() {
		IntegerItem number = ItemFactory.newIntegerItem("number", "number", null);
		number.setWrapTitle(false);
		number.setRequired(true);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), number, new SpacerItem());

		return form;
	}

	private DynamicForm prepareDatesForm() {
		DateItem date = ItemFactory.newDateItem("date", "date");
		date.setWrapTitle(false);
		date.setRequired(true);
		date.setValue(new Date());
		date.setDisabled(true);

		CheckboxItem currentDate = ItemFactory.newCheckbox("currentdate", "currentdate");
		currentDate.setWrapTitle(false);
		currentDate.setValue(true);
		currentDate.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				date.setDisabled(currentDate.getValueAsBoolean());
			}
		});

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), currentDate, date, new SpacerItem());

		return form;
	}

	private DynamicForm prepareTextForm() {
		TextItem value = ItemFactory.newTextItem("value", "value", null);
		value.setWidth(500);
		value.setRequired(true);
		value.setWrapTitle(false);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), value, new SpacerItem());

		return form;
	}
}