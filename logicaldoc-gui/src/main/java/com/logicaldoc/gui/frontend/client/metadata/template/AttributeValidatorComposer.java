package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.Date;
import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FloatItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A visual editor for validation procedures
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.1
 */
public class AttributeValidatorComposer extends Window {

	private ValuesManager vm = new ValuesManager();

	private FormItem sourceItem = null;

	private int attributeType = GUIAttribute.TYPE_STRING;

	public AttributeValidatorComposer(FormItem sourceItem, int attributeType) {
		super();

		this.sourceItem = sourceItem;
		this.attributeType = attributeType;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("validatorcomposer"));
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
		if (attributeType == GUIAttribute.TYPE_INT || attributeType == GUIAttribute.TYPE_DOUBLE)
			wizardForm = prepareNumbersForm();
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
			Date min = (Date) vm.getValue("date-min");
			String minStr = min != null ? DateUtil.format(min, "yyyy-MM-dd") : null;
			Date max = (Date) vm.getValue("date-max");
			String maxStr = min != null ? DateUtil.format(max, "yyyy-MM-dd") : null;

			if (minStr != null) {
				sb.append("#set($min = $DateTool.parse('2022-06-01', 'yyyy-MM-dd'))\n");
				sb.append("#if($min.after($value)) $error.setDescription($I18N.get('dateoutofrange')); #end\n\n");
			}
			if (maxStr != null) {
				sb.append("#set($max = $DateTool.parse('2022-06-30', 'yyyy-MM-dd'))\n");
				sb.append("#if($max.before($value)) $error.setDescription($I18N.get('dateoutofrange')); #end\n\n");
			}
		} else if (attributeType == GUIAttribute.TYPE_INT || attributeType == GUIAttribute.TYPE_DOUBLE) {
			Float min = vm.getValueAsString("number-min") != null ? Float.valueOf(vm.getValueAsString("number-min"))
					: null;
			Float max = vm.getValueAsString("number-max") != null ? Float.valueOf(vm.getValueAsString("number-max"))
					: null;

			if (min != null) {
				sb.append("#set($min = " + min + ")\n");
				sb.append("#if($min > $value) $error.setDescription($I18N.get('numberoutofrange')); #end\n\n");
			}
			if (max != null) {
				sb.append("#set($max = " + max + ")\n");
				sb.append("#if($max < $value) $error.setDescription($I18N.get('numberoutofrange')); #end\n\n");
			}
		} else if (attributeType == GUIAttribute.TYPE_STRING || attributeType == GUIAttribute.TYPE_STRING_PRESET
				|| attributeType == GUIAttribute.TYPE_STRING_TEXTAREA) {
			String regexp = vm.getValueAsString("text-regexp");
			sb.append("#if(!$value.matches('" + regexp
					+ "')) $error.setDescription($I18N.get('invalidformat')); #end\n\n");
		}

		return sb.toString();
	}

	private DynamicForm prepareNumbersForm() {
		FloatItem min = ItemFactory.newFloatItem("number-min", "minimumvalue", null);
		min.setWrapTitle(false);
		FloatItem max = ItemFactory.newFloatItem("number-max", "maximumvalue", null);
		max.setWrapTitle(false);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), min, max, new SpacerItem());

		return form;
	}

	private DynamicForm prepareDatesForm() {
		DateItem min = ItemFactory.newDateItem("date-min", "minimumdate");
		min.setWrapTitle(false);
		DateItem max = ItemFactory.newDateItem("date-max", "maximumdate");
		max.setWrapTitle(false);

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), min, max, new SpacerItem());

		return form;
	}

	private DynamicForm prepareTextForm() {
		TextItem regexp = ItemFactory.newTextItem("text-regexp", "rregularexpression", null);
		regexp.setWidth(500);
		regexp.setRequired(true);
		regexp.setWrapTitle(false);

		SelectItem patterns = ItemFactory.newSelectItem("text-patterns", "commonexpressions");
		LinkedHashMap<String, String> opts = new LinkedHashMap<String, String>();
		opts.put("^([\\w-\\.]+){1,64}@([\\w&&[^_]]+){2,255}.[a-z]{2,}$", I18N.message("email"));
		opts.put("^(https?|ftps?|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]$", I18N.message("url"));
		patterns.setValueMap(opts);
		patterns.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				regexp.setValue(event.getValue());
			}
		});

		DynamicForm form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setFields(new SpacerItem(), patterns, regexp, new SpacerItem());

		return form;
	}
}