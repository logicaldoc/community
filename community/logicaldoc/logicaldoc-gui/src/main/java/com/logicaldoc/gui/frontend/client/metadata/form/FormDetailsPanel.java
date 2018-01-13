package com.logicaldoc.gui.frontend.client.metadata.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects details about a form
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FormDetailsPanel extends VLayout {
	private GUIDocument form;

	private Layout propertiesTabPanel;

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private FormsPanel formsPanel;

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form1 = null;

	public FormDetailsPanel(FormsPanel formsPanel) {
		super();

		this.formsPanel = formsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		Button saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(2);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("70%");
		spacer.setOverflow(Overflow.HIDDEN);

		Img closeImage = ItemFactory.newImgIcon("delete.png");
		closeImage.setHeight("16px");
		closeImage.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (form.getId() != 0) {
					DocumentService.Instance.get().getById(form.getId(), new AsyncCallback<GUIDocument>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument Form) {
							setForm(Form);
						}
					});
				} else {
					GUIDocument newForm = new GUIDocument();
					setForm(newForm);
				}
				savePanel.setVisible(false);
			}
		});
		closeImage.setCursor(Cursor.HAND);
		closeImage.setTooltip(I18N.message("close"));
		closeImage.setLayoutAlign(Alignment.RIGHT);
		closeImage.setLayoutAlign(VerticalAlignment.CENTER);

		savePanel.addMember(saveButton);
		savePanel.addMember(spacer);
		savePanel.addMember(closeImage);
		addMember(savePanel);
	}

	private void refresh() {
		vm.clearErrors(false);
		vm.clearValues();
		vm.resetValues();

		if (form1 != null)
			form1.destroy();

		if (savePanel != null)
			savePanel.setVisible(false);

		/*
		 * Prepare the standard properties tab
		 */
		if (tabSet != null)
			removeMember(tabSet);

		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setAlign(VerticalAlignment.TOP);
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		addMember(tabSet);

		ChangedHandler changedHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		form1 = new DynamicForm();
		form1.setNumCols(2);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);

		TextItem name = ItemFactory.newSimpleTextItem("name", "name", form.getFileName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);
		name.setDisabled(form.getId() != 0L);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);
		template.addChangedHandler(changedHandler);
		template.setMultiple(false);
		if (form.getTemplateId() != null)
			template.setValue(form.getTemplateId().toString());

		form1.setItems(name, template);
		propertiesTabPanel.setMembers(form1);
	}

	public GUIDocument getForm() {
		return form;
	}

	public void setForm(GUIDocument form) {
		this.form = form;
		refresh();
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	public void onSave() {
		if (vm.validate()) {
			form.setFileName(vm.getValueAsString("name"));
			if (vm.getValueAsString("template") != null)
				form.setTemplateId(Long.parseLong(vm.getValueAsString("template")));
			else
				form.setTemplateId(null);
			form.setNature(Constants.NATURE_FORM);

			DocumentService.Instance.get().save(form, new AsyncCallback<GUIDocument>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIDocument newForm) {
					savePanel.setVisible(false);
					if (form.getId() == 0L)
						formsPanel.init();
					else if (newForm != null) {
						formsPanel.updateRecord(newForm);
						formsPanel.showFormDetails(newForm);
					}
				}
			});
		}
	}
}