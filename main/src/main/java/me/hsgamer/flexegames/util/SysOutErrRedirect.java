package me.hsgamer.flexegames.util;

import lombok.experimental.UtilityClass;
import net.minestom.server.MinecraftServer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.OutputStream;
import java.io.PrintStream;

@UtilityClass
public final class SysOutErrRedirect {
    public static void init() {
        System.setOut(new SysPrintStream(System.out, false, "[SYSOUT] "));
        System.setErr(new SysPrintStream(System.err, true, "[SYSERR] "));
    }

    private static class SysPrintStream extends PrintStream {
        private final boolean isError;
        private final String prefix;

        private SysPrintStream(@NotNull OutputStream out, boolean isError, String prefix) {
            super(out);
            this.isError = isError;
            this.prefix = prefix;
        }

        @Override
        public void println(@Nullable String x) {
            if (x == null) {
                x = "null";
            }
            String format = prefix + x;
            if (isError) {
                MinecraftServer.LOGGER.error(format);
            } else {
                MinecraftServer.LOGGER.info(format);
            }
        }
    }
}
