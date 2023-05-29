package com.logicaldoc.gui.frontend.client.reports.custom;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ReportService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This popup window is used to fill the report parameters
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class ReportParametersForm extends Window {

	private DynamicForm form = new DynamicForm();

	private GUIReport report;

	private GUIAttribute[] parameters;

	private CustomReportsPanel panel;

	public ReportParametersForm(GUIReport form, CustomReportsPanel panel) {
		this.report = form;
		this.panel = panel;

		setTitle(I18N.message("form") + " - " + form.getName());
		setWidth(500);
		setHeight(400);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		ReportService.Instance.get().getReportParameters(form.getId(), new AsyncCallback<GUIAttribute[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAttribute[] parameters) {
				ReportParametersForm.this.parameters = parameters;
				initGUI();
			}
		});
	}

	private void initGUI() {
		VLayout layout = new VLayout();
		layout.setMargin(5);
		layout.setMembersMargin(3);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		List<FormItem> items = new ArrayList<>();
		for (GUIAttribute att : parameters) {
			if (att.getType() == GUIAttribute.TYPE_STRING) {
				FormItem item = ItemFactory.newStringItemForAttribute(att);
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_INT) {
				FormItem item = ItemFactory.newIntegerItemForAttribute(att.getName(), att.getLabel(), null);
				item.setRequired(att.isMandatory());
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_BOOLEAN) {
				SelectItem item = ItemFactory.newBooleanSelectorForAttribute(att.getName(), att.getLabel(),
						!att.isMandatory());
				item.setRequired(att.isMandatory());
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_DOUBLE) {
				FormItem item = ItemFactory.newFloatItemForAttribute(att.getName(), att.getLabel(), null);
				item.setRequired(att.isMandatory());
				items.add(item);
			} else if (att.getType() == GUIAttribute.TYPE_DATE) {
				final DateItem item = ItemFactory.newDateItemForAttribute(att.getName(), att.getLabel());
				item.setRequired(att.isMandatory());
				item.addKeyPressHandler(event -> {
					if ("backspace".equals(event.getKeyName().toLowerCase())
							|| "delete".equals(event.getKeyName().toLowerCase())) {
						item.clearValue();
						item.setValue((Date) null);
					}
				});
				items.add(item);
			}
		}

		IButton execute = new IButton();
		execute.setTitle(I18N.message("execute"));
		execute.setAutoFit(true);
		execute.addClickHandler(event -> onExecute());

		HLayout buttonsBar = new HLayout();
		buttonsBar.setWidth100();
		buttonsBar.setHeight(25);
		buttonsBar.setMembers(execute);

		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setItems(items.toArray(new FormItem[0]));

		Tab tab = new Tab(I18N.message("reportparams"));

		if (items.isEmpty()) {
			Label label = new Label(I18N.message("reportnoparams"));
			label.setAlign(Alignment.CENTER);
			tab.setPane(label);
		} else
			tab.setPane(form);

		TabSet tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();
		tabSet.addTab(tab);

		layout.setMembers(tabSet, buttonsBar);

		addItem(layout);
	}

	private GUIAttribute getParameter(String name) {
		for (GUIAttribute param : parameters) {
			if (param.getName().equals(name))
				return param;
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public void onExecute() {
		if (!form.validate())
			return;

		Map<String, Object> values = (Map<String, Object>) form.getValues();

		ArrayList<GUIAttribute> params = new ArrayList<>();

		for (String name : values.keySet()) {
			if (!name.startsWith("_"))
				continue;

			Object value = values.get(name);
			String parameterName = name.substring(1).replace(Constants.BLANK_PLACEHOLDER, " ");
			GUIAttribute attribute = getParameter(parameterName);
			if (attribute == null)
				continue;

			if (value != null) {
				setAttributeValue(value, parameterName, attribute);
				params.add(attribute);
			} else {
				attribute.setIntValue(null);
				attribute.setBooleanValue(null);
				attribute.setDoubleValue(null);
				attribute.setDateValue(null);
				attribute.setStringValue("");
			}
		}

		doExecute(params);
	}

	private void setAttributeValue(Object value, String parameterName, GUIAttribute attribute) {
		if (attribute.getType() == GUIAttribute.TYPE_BOOLEAN) {
			if (!(value == null || "".equals(value.toString().trim())))
				attribute.setValue("1".equals(value.toString().trim()) ? true : false);
			else if (getParameter(parameterName) != null) {
				GUIAttribute at = getParameter(parameterName);
				if (at != null) {
					at.setBooleanValue(null);
					at.setType(GUIAttribute.TYPE_BOOLEAN);
				}
			}
		} else
			attribute.setValue(value);
	}

	private void doExecute(ArrayList<GUIAttribute> parameters) {
		ReportService.Instance.get().execute(report.getId(), parameters.toArray(new GUIAttribute[0]),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						destroy();
						GuiLog.info(I18N.message("reportinexecution"), null);
						panel.update();
					}
				});
	}
}