package com.logicaldoc.gui.frontend.client.reports.custom;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FolderSelector;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.ReportService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Uploads and creates a new Report
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class ReportUploader extends Window {

	private IButton save;

	private Upload uploader;

	private ValuesManager vm;

	private FolderSelector outputFolderSelector;

	private DynamicForm form;

	private CustomReportsPanel reportsPanel;

	public ReportUploader(CustomReportsPanel reportsPanel, final GUIReport report) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (report != null)
			setTitle(I18N.message("uploadnewdesign") + " - " + report.getName());
		else
			setTitle(I18N.message("newreport"));
		setWidth(460);

		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		this.reportsPanel = reportsPanel;

		save = new IButton(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave(report);
			}
		});

		VLayout layout = new VLayout();
		layout.setMembersMargin(2);
		layout.setMargin(2);

		if (report == null) {
			prepareForm();
			layout.addMember(form);
		}

		uploader = new Upload(save);
		layout.addMember(uploader);
		layout.addMember(save);

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});

		addItem(layout);

		// Clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				// Nothing to do
			}
		});
	}

	private void prepareForm() {
		form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		outputFolderSelector = new FolderSelector("outputFolder", false);
		outputFolderSelector.setRequired(true);
		outputFolderSelector.setWidth(250);
		outputFolderSelector.setTitle(I18N.message("outputfolder"));

		TextItem name = ItemFactory.newSimpleTextItem("name", null);
		name.setRequired(true);

		form.setItems(name, outputFolderSelector);
	}

	public void onSave(GUIReport report) {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		if (report != null) {
			ReportService.Instance.get().storeUploadedDesign(report.getId(), new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg) {
					destroy();
					I18N.message("reportsaved");
				}
			});
		} else {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			report = new GUIReport();
			report.setName(vm.getValueAsString("name"));
			report.setOutputFolder(outputFolderSelector.getFolder());

			ReportService.Instance.get().create(report, new AsyncCallback<GUIReport>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIReport rep) {
					I18N.message("reportsaved");
					reportsPanel.refresh();
					destroy();
				}
			});
		}
	}
}