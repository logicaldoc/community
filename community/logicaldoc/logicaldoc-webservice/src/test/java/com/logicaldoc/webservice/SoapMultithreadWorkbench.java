package com.logicaldoc.webservice;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.webservice.model.WSAttribute;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.soap.client.SoapAuthClient;
import com.logicaldoc.webservice.soap.client.SoapDocumentClient;
import com.logicaldoc.webservice.soap.client.SoapFolderClient;
import com.logicaldoc.webservice.soap.client.SoapSystemClient;

/**
 * A collection of tests thought to stress a LogicalDOC instance overhelming it
 * with a lot of operations over the same documents/folders to detect possible
 * concurrency problems in the data layer.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class SoapMultithreadWorkbench extends SoapWorkbench {
	private static int threads = 5;

	private static ExecutorService executorService = Executors.newFixedThreadPool(threads);

	public static void main(String[] args) throws Exception {
		SoapAuthClient auth = new SoapAuthClient(BASE + "/Auth");

		SoapSystemClient systemClient = new SoapSystemClient(BASE + "/System");

		// Open a session
		String sid = auth.login("admin", "12345678");
		System.out.println("Server date: " + systemClient.getInfo().getDate());
		System.out.println("Sid: " + sid);

		List<TestCallable> tasks = new ArrayList<TestCallable>();
		for (int i = 0; i < threads; i++) {
			// tasks.add(updateMetadata(sid));
			// tasks.add(moveToFolder(sid));
			// tasks.add(createDocument(sid));
			// tasks.add(lockDocument(sid));
			// tasks.add(checkinkDocument(sid));
			tasks.add(renameDocument(sid));
		}

		executorService.invokeAll(tasks);

		boolean allDone = false;
		while (!allDone) {
			allDone = true;
			for (TestCallable task : tasks) {
				if (!task.isFinished())
					allDone = false;
			}

			System.out.println("alldone: " + allDone);
		}

		executorService.shutdown();
	}

	private static TestCallable moveToFolder(String sid) throws Exception {
		SoapFolderClient folderClient = new SoapFolderClient(BASE + "/Folder", 1, false, 50);

		final WSFolder folder1 = folderClient.getFolder(sid, 4L);
		final WSFolder folder2 = folderClient.getFolder(sid, 717511364L);

		return new TestCallable(sid) {
			@Override
			public String call() throws Exception {
				long docId = 723655194L;
				try {
					SoapDocumentClient documentClient = new SoapDocumentClient(
							SoapMultithreadWorkbench.BASE + "/Document");

					for (int i = 0; i < 100; i++) {
						WSDocument wsDoc = documentClient.getDocument(sid, docId);
						documentClient.move(sid, docId,
								wsDoc.getFolderId() == folder1.getId() ? folder2.getId() : folder1.getId());

						System.out.println("++ move " + i);
					}
				} catch (Throwable t) {
					if (t.getMessage().contains("org.hibernate"))
						t.printStackTrace();
				}

				return super.call();
			}
		};
	}

	private static TestCallable checkinkDocument(String sid) throws Exception {
		SoapDocumentClient documentClient = new SoapDocumentClient(SoapMultithreadWorkbench.BASE + "/Document");

		return new TestCallable(sid) {
			@Override
			public String call() throws Exception {
				long docId = 723655194L;
				try {
					for (int i = 0; i < 100; i++) {
						WSDocument wsDoc = documentClient.getDocument(sid, docId);
						if (wsDoc.getStatus() == WSDocument.DOC_UNLOCKED) {
							System.out.println("## checkout " + i);
							documentClient.checkout(sid, docId);
						} else {
							File tmp = File.createTempFile("wstest", wsDoc.getFileName());
							try {
								System.out.println("## checkin " + i);
								documentClient.downloadContent(sid, docId, tmp);
								documentClient.checkin(sid, docId, "soap massive test", wsDoc.getFileName(), false,
										tmp);
							} finally {
								FileUtil.strongDelete(tmp);
							}
						}

					}
				} catch (Throwable t) {
					if (t.getMessage().contains("org.hibernate"))
						t.printStackTrace();
				}

				return super.call();
			}
		};
	}

	private static TestCallable renameDocument(String sid) throws Exception {
		SoapDocumentClient documentClient = new SoapDocumentClient(SoapMultithreadWorkbench.BASE + "/Document");

		return new TestCallable(sid) {
			@Override
			public String call() throws Exception {
				long docId = 723655194L;
				try {
					for (int i = 0; i < 100; i++) {
						WSDocument wsDoc = documentClient.getDocument(sid, docId);
						documentClient.rename(sid, docId,
								wsDoc.getFileName().contains("10_29") ? "quietanza_2019_11_29.pdf"
										: "quietanza_2019_10_29.pdf");

						System.out.println("-- rename " + i);
					}
				} catch (Throwable t) {
					if (t.getMessage().contains("org.hibernate"))
						t.printStackTrace();
				}

				return super.call();
			}
		};
	}

	private static TestCallable lockDocument(String sid) throws Exception {
		SoapDocumentClient documentClient = new SoapDocumentClient(SoapMultithreadWorkbench.BASE + "/Document");

		return new TestCallable(sid) {
			@Override
			public String call() throws Exception {
				long docId = 723655194L;
				try {
					for (int i = 0; i < 100; i++) {
						WSDocument wsDoc = documentClient.getDocument(sid, docId);
						if (wsDoc.getStatus() == WSDocument.DOC_UNLOCKED)
							documentClient.checkout(sid, docId);
						else
							documentClient.unlock(sid, docId);

						System.out.println("++ lock " + i);
					}
				} catch (Throwable t) {
					if (t.getMessage().contains("org.hibernate"))
						t.printStackTrace();
				}

				return super.call();
			}
		};
	}

	private static TestCallable updateMetadata(String sid) throws Exception {
		SoapDocumentClient documentClient = new SoapDocumentClient(SoapMultithreadWorkbench.BASE + "/Document");

		return new TestCallable(sid) {
			@Override
			public String call() throws Exception {
				long docId = 723655194L;
				try {
					for (int i = 0; i < 100; i++) {
						WSDocument wsDoc = documentClient.getDocument(sid, docId);
						WSAttribute[] attributes = wsDoc.getAttributes();
						Double value = null;
						for (WSAttribute att : attributes) {
							if (att.getName().equals("Total")) {
								att.setValue(att.getDoubleValue() + 1);
								value = att.getDoubleValue();
							}
						}

						documentClient.update(sid, wsDoc);

						System.out.println("++ update " + i + ": " + value);
					}
				} catch (Throwable t) {
					if (t.getMessage().contains("org.hibernate"))
						t.printStackTrace();
				}

				return super.call();
			}
		};
	}

	private static TestCallable createDocument(String sid) throws Exception {
		SoapDocumentClient documentClient = new SoapDocumentClient(SoapMultithreadWorkbench.BASE + "/Document");

		return new TestCallable(sid) {
			@Override
			public String call() throws Exception {
				try {
					for (int i = 0; i < 10; i++) {
						SoapFolderClient folderClient = new SoapFolderClient(BASE + "/Folder", 1, false, 50);
						WSFolder folder = folderClient.createPath(sid, Folder.DEFAULTWORKSPACEID, "Test");

						WSDocument wsDoc = new WSDocument();
						wsDoc.setFileName("pom.xml");
						wsDoc.setFolderId(folder.getId());
						wsDoc.setTemplateId(547815424L);

						WSAttribute att1 = new WSAttribute();
						att1.setName("industries");
						att1.setValue("Banking");
						wsDoc.addAttribute(att1);

						WSAttribute att2 = new WSAttribute();
						att2.setName("industries-001");
						att2.setValue("Accounting");
						att2.setParent("industries");
						wsDoc.addAttribute(att2);

						documentClient.create(sid, wsDoc, new File("pom.xml"));

						System.out.println("++ create " + i);
					}
				} catch (Throwable t) {
					if (t.getMessage().contains("org.hibernate"))
						t.printStackTrace();
				}

				return super.call();
			}
		};
	}

	static class TestCallable implements Callable<String> {
		protected String sid;

		protected boolean finished = false;

		public boolean isFinished() {
			return finished;
		}

		public TestCallable(String sid) {
			super();
			this.sid = sid;
		}

		@Override
		public String call() throws Exception {
			finished = true;
			return "ok";
		}
	}
}
