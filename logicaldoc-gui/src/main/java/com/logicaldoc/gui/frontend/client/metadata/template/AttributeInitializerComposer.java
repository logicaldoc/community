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
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A visual editor for initialization routines
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class AttributeInitializerComposer extends Window {

	private static final String OFFSET = "offset";

	private static final String CURRENTDATE = "currentdate";

	private static final String NUMBER = "number";

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
		save.addClickHandler(event -> {
			if (sourceItem != null && vm.validate()) {
				sourceItem.clearErrors();
				sourceItem.setValue(composeAutomation());
				destroy();
			}
		});
		save.setDisabled(sourceItem == null || sourceItem.isDisabled());

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> destroy());

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
			wizardForm = prepareDateForm();
		else if (attributeType == GUIAttribute.TYPE_STRING || attributeType == GUIAttribute.TYPE_STRING_PRESET
				|| attributeType == GUIAttribute.TYPE_STRING_TEXTAREA)
			wizardForm = prepareTextForm();
		addItem(wizardForm);
		addItem(toolStrip);
	}

	protected String composeAutomation() {
		StringBuilder stringBuilder = new StringBuilder();

		if (attributeType == GUIAttribute.TYPE_DATE) {
			initializeDate(stringBuilder);
		} else if (attributeType == GUIAttribute.TYPE_INT) {
			Long number = Long.valueOf(vm.getValueAsString(NUMBER));
			stringBuilder.append("#set($number = " + number + ")\n");
			stringBuilder.append("$attribute.setIntValue($number);");

		} else if (attributeType == GUIAttribute.TYPE_DOUBLE) {
			Double number = Double.valueOf(vm.getValueAsString(NUMBER));
			stringBuilder.append("#set($number = " + number + ")\n");
			stringBuilder.append("$attribute.setDoubleValue($number);");
		} else if (attributeType == GUIAttribute.TYPE_STRING || attributeType == GUIAttribute.TYPE_STRING_PRESET
				|| attributeType == GUIAttribute.TYPE_STRING_TEXTAREA) {
			String text = vm.getValueAsString("value");
			stringBuilder.append("$attribute.setStringValue('" + text + "');");
		}

		return stringBuilder.toString();
	}

	private void initializeDate(StringBuilder stringBuilder) {
		Date date = (Date) vm.getValue("date");
		boolean currentdate = vm.getValue(CURRENTDATE) != null ? (Boolean) vm.getValue(CURRENTDATE) : false;
		if (currentdate) {
			stringBuilder.append("#set($dt = $CURRENT_DATE)\n");
		} else {
			String dateStr = date != null ? DateUtil.format(date, "yyyy-MM-dd") : null;
			stringBuilder.append("#set($dt = $DateTool.parse('" + dateStr + "', 'yyyy-MM-dd'))\n");
		}

		int offset = vm.getValue(OFFSET) != null ? (Integer) vm.getValue(OFFSET) : 0;
		if (offset != 0)
			stringBuilder.append("#set($dt = $DateTool.addDays($dt, " + offset + "))\n");

		stringBuilder.append("$attribute.setDateValue($dt);");
	}

	private DynamicForm prepareFloatForm() {
		FloatItem number = ItemFactory.newFloatItem(NUMBER, NUMBER, null);
		number.setWrapTitle(false);
		number.setRequired(true);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), number, new SpacerItem());

		return form;
	}

	private DynamicForm prepareIntegerForm() {
		IntegerItem number = ItemFactory.newIntegerItem(NUMBER, NUMBER, null);
		number.setWrapTitle(false);
		number.setRequired(true);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), number, new SpacerItem());

		return form;
	}

	private DynamicForm prepareDateForm() {
		DateItem date = ItemFactory.newDateItem("date");
		date.setWrapTitle(false);
		date.setRequired(true);
		date.setValue(new Date());
		date.setDisabled(true);

		CheckboxItem currentDate = ItemFactory.newCheckbox(CURRENTDATE, CURRENTDATE);
		currentDate.setWrapTitle(false);
		currentDate.setValue(true);
		currentDate.addChangedHandler(event -> date.setDisabled(currentDate.getValueAsBoolean()));

		SpinnerItem offset = ItemFactory.newSpinnerItem(OFFSET, 0);
		offset.setWrapTitle(false);
		offset.setMin(Integer.MIN_VALUE);
		offset.setHint(I18N.message("days"));

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), currentDate, date, offset, new SpacerItem());

		return form;
	}

	private DynamicForm prepareTextForm() {
		TextItem value = ItemFactory.newTextItem("value", null);
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