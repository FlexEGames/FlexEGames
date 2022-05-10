package me.hsgamer.flexgames.config.path;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;

public class StringComponentListPath extends ComponentListPath {
    public StringComponentListPath(@NotNull String path, @Nullable List<String> def) {
        super(path, Optional.ofNullable(def).map(list -> list.stream().map(LegacyComponentSerializer.legacyAmpersand()::deserialize).map(Component::asComponent).toList()).orElse(null));
    }
}
