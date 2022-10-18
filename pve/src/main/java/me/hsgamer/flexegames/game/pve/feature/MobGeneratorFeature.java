package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.flexegames.feature.ConfigFeature;
import me.hsgamer.flexegames.feature.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveGameConfig;
import me.hsgamer.flexegames.game.pve.mob.*;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.ArenaFeature;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.implementation.feature.arena.ArenaTimerFeature;
import net.minestom.server.coordinate.Vec;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.function.*;

import static me.hsgamer.flexegames.game.pve.PveGame.HEIGHT;
import static me.hsgamer.flexegames.game.pve.PveGame.SPAWN_RADIUS;

public class MobGeneratorFeature extends ArenaFeature<MobGeneratorFeature.ArenaMobGeneratorFeature> {
    private static final List<MobGenerator> MOB_GENERATORS = List.of(
            MobGenerator.of(ZombieMob::new)
                    .chance(0.5),
            MobGenerator.of(SpiderMob::new)
                    .chance(0.33)
                    .minStage(2)
                    .controller(count -> count <= 2),
            MobGenerator.of(SkeletonMob::new)
                    .chance(0.25)
                    .minStage(4),
            MobGenerator.of(BlazeMob::new)
                    .chance(0.1)
                    .minStage(6)
                    .preference(arena -> arena.getArenaFeature(JoinFeature.class).getPlayerCount() >= 2 ? 1 : 0.5) // Prefer a group size of 2 or more
                    .controller(count -> count <= 2),
            MobGenerator.of(EndermanMob::new)
                    .chance(0.05)
                    .minStage(8)
                    // Prefer a group size of 2 or more
                    .preference(arena -> arena.getArenaFeature(JoinFeature.class).getPlayerCount() >= 2 ? 1 : 0.5)
                    // +1 max every 10 stages
                    .controller((arena, count) -> count <= arena.getArenaFeature(StageFeature.class).getStage() / 10),
            MobGenerator.of(EvokerMob::new)
                    .chance(0.1)
                    .minStage(10)
                    // Prefer a group size of 3 or more
                    .preference(arena -> arena.getArenaFeature(JoinFeature.class).getPlayerCount() >= 3 ? 1 : 0.5)
                    // +1 max every 10 stages
                    .controller((arena, count) -> count <= arena.getArenaFeature(StageFeature.class).getStage() / 10)
    );

    @Override
    protected ArenaMobGeneratorFeature createFeature(Arena arena) {
        return new ArenaMobGeneratorFeature(arena);
    }

    public static class ArenaMobGeneratorFeature implements Feature {
        private final Arena arena;
        private final Queue<ArenaMob> spawningMobs = new LinkedList<>();
        private final List<ArenaMob> spawnedMobs = new LinkedList<>();

        public ArenaMobGeneratorFeature(Arena arena) {
            this.arena = arena;
        }

        public void loadMobs() {
            var random = ThreadLocalRandom.current();
            var stage = arena.getArenaFeature(StageFeature.class).getStage();
            var gameConfig = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class);
            int initialMobCount = Math.min((int) (stage * 1.5) * (gameConfig.isMayhem() ? 10 : 1), 200);

            List<ArenaMob> initialMobs = new ArrayList<>();
            while (initialMobs.size() < initialMobCount) {
                var generator = MOB_GENERATORS.get(random.nextInt(MOB_GENERATORS.size()));
                generator.generate(arena, initialMobs.size()).ifPresent(initialMobs::add);
            }

            spawningMobs.addAll(initialMobs);
        }

        public void spawnMobs() {
            var mobPerSpawn = arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getMobPerSpawn();
            var instance = arena.getArenaFeature(InstanceFeature.class).getInstance();
            for (int i = 0; i < mobPerSpawn; i++) {
                if (spawningMobs.isEmpty()) {
                    break;
                }
                ArenaMob mob = spawningMobs.poll();
                mob.setInstance(instance, Vec.ONE
                        .rotateAroundY(ThreadLocalRandom.current().nextDouble(2 * Math.PI))
                        .mul(SPAWN_RADIUS, 0, SPAWN_RADIUS)
                        .asPosition()
                        .add(0, HEIGHT, 0));
                spawnedMobs.add(mob);
            }
        }

        public void trySpawnMobs() {
            var timerFeature = arena.getArenaFeature(ArenaTimerFeature.class);
            if (timerFeature.getDuration(TimeUnit.MILLISECONDS) > 0) return;
            spawnMobs();
            timerFeature.setDuration(
                    arena.getFeature(ConfigFeature.class).getConfig(PveGameConfig.class).getSpawnDelay(),
                    TimeUnit.MILLISECONDS
            );
        }

        public void clearMobs() {
            spawningMobs.clear();
            spawnedMobs.forEach(ArenaMob::remove);
            spawnedMobs.clear();
        }

        public boolean isCleared() {
            if (!spawningMobs.isEmpty()) return false;
            spawnedMobs.removeIf(ArenaMob::isDead);
            return spawnedMobs.isEmpty();
        }

        public int getMobCount() {
            return spawningMobs.size() + spawnedMobs.size();
        }

        @Override
        public void clear() {
            clearMobs();
        }
    }

    private static class MobGenerator {
        private final Function<Arena, ArenaMob> creator;
        private final List<BiPredicate<Arena, Integer>> controllers = new LinkedList<>();
        private final List<Pair<ToDoubleBiFunction<Arena, Integer>, Double>> preferences = new LinkedList<>();

        private MobGenerator(Function<Arena, ArenaMob> creator) {
            this.creator = creator;
        }

        public static MobGenerator of(Function<Arena, ArenaMob> creator) {
            return new MobGenerator(creator);
        }

        public MobGenerator controller(BiPredicate<Arena, Integer> controller) {
            controllers.add(controller);
            return this;
        }

        public MobGenerator controller(IntPredicate controller) {
            return controller((arena, count) -> controller.test(count));
        }

        public MobGenerator condition(Predicate<Arena> condition) {
            return controller((arena, count) -> condition.test(arena));
        }

        public MobGenerator chance(double chance) {
            return condition(arena -> Math.random() < chance);
        }

        public MobGenerator preferStage(IntPredicate stagePredicate) {
            return condition(arena -> stagePredicate.test(arena.getArenaFeature(StageFeature.class).getStage()));
        }

        public MobGenerator minStage(int minStage) {
            return preferStage(stage -> stage >= minStage);
        }

        public MobGenerator preferPlayers(IntPredicate playerPredicate) {
            return condition(arena -> playerPredicate.test(arena.getArenaFeature(JoinFeature.class).getPlayerCount()));
        }

        public MobGenerator minPlayers(int minPlayers) {
            return preferPlayers(count -> count >= minPlayers);
        }

        public MobGenerator preference(ToDoubleBiFunction<Arena, Integer> preference, double weight) {
            preferences.add(Pair.of(preference, weight));
            return this;
        }

        public MobGenerator preference(ToDoubleBiFunction<Arena, Integer> preference) {
            return preference(preference, 1);
        }

        public MobGenerator preference(ToDoubleFunction<Arena> preference, double weight) {
            return preference((arena, count) -> preference.applyAsDouble(arena), weight);
        }

        public MobGenerator preference(ToDoubleFunction<Arena> preference) {
            return preference(preference, 1);
        }

        public Optional<ArenaMob> generate(Arena arena, int currentMobCount) {
            if (controllers.stream().anyMatch(controller -> !controller.test(arena, currentMobCount))) {
                return Optional.empty();
            }
            double score = 0;
            double weight = 0;
            for (Pair<ToDoubleBiFunction<Arena, Integer>, Double> preference : preferences) {
                score += preference.getKey().applyAsDouble(arena, currentMobCount);
                weight += preference.getValue();
            }
            if (weight > 0 && Math.random() > score / weight) {
                return Optional.empty();
            }
            return Optional.of(creator.apply(arena));
        }
    }
}
