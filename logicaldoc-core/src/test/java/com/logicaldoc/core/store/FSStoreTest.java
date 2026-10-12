package com.logicaldoc.core.store;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.util.plugin.PluginException;

public class FSStoreTest extends AbstractCoreTestCase {

    private static final long DOC_ID = 1L;

    // Instance under test
    private FSStore testSubject;

    private File canary;

    @Before
    @Override
    public void setUp() throws IOException, SQLException, PluginException {
        super.setUp();

        // Store 1 (store.1.dir), prepared by AbstractCoreTestCase with
        // document 1 stored in 1/doc/1.0
        testSubject = new FSStore();

        // A file outside any document container that must never be reachable
        canary = new File(rootStoreOne, "canary.txt");
        Files.write(canary.toPath(), "secret".getBytes(StandardCharsets.UTF_8));
    }

    @Test
    public void testValidResource() throws IOException {
        StoreResource res = StoreResource.builder().docId(DOC_ID).fileVersion("1.0").build();
        File file = new File("%s/1/doc/1.0".formatted(rootStoreOne.getPath()));
        byte[] expected = Files.readAllBytes(file.toPath());

        assertTrue(testSubject.exists(res));
        assertEquals(file.length(), testSubject.size(res));
        try (InputStream is = testSubject.getStream(res)) {
            assertArrayEquals(expected, is.readAllBytes());
        }

        res = StoreResource.builder().docId(DOC_ID).fileVersion("1.0").suffixPdfConversion().build();
        assertTrue(testSubject.exists(res));
    }

    // Traversal attempts must be rejected
    @Test
    public void testParentTraversalInSuffix() {
        // Walk up from the container to the store root, where the canary is
        String up = "../".repeat(depthOfContainer());
        assertRejectedEverywhere("1.0", up + "canary.txt");
    }

    @Test
    public void testBackslashTraversalInSuffix() {
        String up = "..\\".repeat(depthOfContainer());
        assertRejectedEverywhere("1.0", up + "canary.txt");
    }

    @Test
    public void testTraversalThroughFakeFolder() {
        // "1.0-.." looks like a folder name, the next ".." cancels it and the
        // third ".." leaves the container
        assertRejectedEverywhere("1.0", "../../../x");
    }

    @Test
    public void testReformedDotDot() {
        // Old sanitizer: "./." has no "..", but stripping "/" re-formed ".."
        assertRejectedEverywhere("1.0", "./.");
        assertRejectedEverywhere("1.0", "./.\\./.\\canary.txt");
    }

    @Test
    public void testTraversalInFileVersion() {
        assertRejectedEverywhere("..", null);
        assertRejectedEverywhere(".", null);
        assertRejectedEverywhere("./.", null);
        assertRejectedEverywhere("../".repeat(depthOfContainer()) + "canary.txt", null);
    }

    // Separators are stripped, the resource stays inside the container
    @Test
    public void testSubfolderIsFlattened() {
        assertStaysInContainer("1.0", "sub/1.0");
    }

    @Test
    public void testAbsolutePathIsFlattened() {
        assertStaysInContainer("1.0", canary.getAbsolutePath());
    }

    private void assertRejectedEverywhere(String fileVersion, String suffix) {
        StoreResource res = StoreResource.builder().docId(DOC_ID).fileVersion(fileVersion).suffix(suffix).build();
        String input = "fileVersion=%s suffix=%s".formatted(fileVersion, suffix);

        assertThrows("exists: " + input, IllegalArgumentException.class, () -> testSubject.exists(res));
        assertThrows("size: " + input, IllegalArgumentException.class, () -> testSubject.size(res));
        assertThrows("getStream: " + input, IllegalArgumentException.class, () -> testSubject.getStream(res));
        assertThrows("getBytes: " + input, IllegalArgumentException.class, () -> testSubject.getBytes(res, 0, 1));
        assertThrows("writeToFile: " + input, IllegalArgumentException.class, () -> {
            File out = File.createTempFile("fsstoretest", ".tmp");
            out.deleteOnExit();
            testSubject.writeToFile(res, out);
        });
        assertThrows("delete: " + input, IllegalArgumentException.class, () -> testSubject.delete(res));

        assertTrue("Canary must survive: " + input, canary.exists());
    }

    private void assertStaysInContainer(String fileVersion, String suffix) {
        StoreResource res = StoreResource.builder().docId(DOC_ID).fileVersion(fileVersion).suffix(suffix).build();
        String name = res.name();

        assertFalse("Separator in name: " + name, name.contains("/") || name.contains("\\"));
        assertEquals(testSubject.getContainer(DOC_ID).getAbsoluteFile(),
                new File(testSubject.getContainer(DOC_ID), name).getAbsoluteFile().getParentFile());

        // The flattened name does not exist, and deleting it must not touch
        // the canary
        assertFalse(testSubject.exists(res));
        testSubject.delete(res);
        assertTrue("Canary must survive: " + name, canary.exists());
    }

    /**
     * Number of folders between the store root and the document container
     */
    private int depthOfContainer() {
        return testSubject.getRoot().toPath().toAbsolutePath().normalize()
                .relativize(testSubject.getContainer(DOC_ID).toPath().toAbsolutePath().normalize()).getNameCount();
    }
}
