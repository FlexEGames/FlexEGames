package me.hsgamer.flexegames.game.pve.feature;

import me.hsgamer.flexegames.feature.arena.JoinFeature;
import me.hsgamer.flexegames.game.pve.PveExtension;
import me.hsgamer.flexegames.game.pve.mob.*;
import me.hsgamer.hscore.common.Pair;
import me.hsgamer.minigamecore.base.Arena;
import me.hsgamer.minigamecore.base.Feature;
import me.hsgamer.minigamecore.implementation.feature.TimerFeature;
import net.minestom.server.coordinate.Vec;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.*;

import static me.hsgamer.flexegames.game.pve.PveGame.HEIGHT;
import static me.hsgamer.flexegames.game.pve.PveGame.SPAWN_RADIUS;

public class MobGeneratorFeature implements Feature {
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
                    .preference(mobArena -> mobArena.getFeature(JoinFeature.class).getPlayerCount() >= 2 ? 1 : 0.5) // Prefer a group size of 2 or more
                    .controller(count -> count <= 2),
            MobGenerator.of(EndermanMob::new)
                    .chance(0.05)
                    .minStage(8)
                    // Prefer a group size of 2 or more
                    .preference(mobArena -> mobArena.getFeature(JoinFeature.class).getPlayerCount() >= 2 ? 1 : 0.5)
                    // +1 max every 10 stages
                    .controller((mobArena, count) -> count <= mobArena.getFeature(StageFeature.class).getStage() / 10),
            MobGenerator.of(EvokerMob::new)
                    .chance(0.1)
                    .minStage(10)
                    // Prefer a group size of 3 or more
                    .preference(mobArena -> mobArena.getFeature(JoinFeature.class).getPlayerCount() >= 3 ? 1 : 0.5)
                    // +1 max every 10 stages
                    .controller((mobArena, count) -> count <= mobArena.getFeature(StageFeature.class).getStage() / 10)
    );

    private final Arena arena;
    private final PveExtension pveExtension;
    private final AtomicInteger maxMobCount = new AtomicInteger();
    private final Queue<ArenaMob> spawningMobs = new LinkedList<>();
    private final List<ArenaMob> spawnedMobs = new LinkedList<>();

    public MobGeneratorFeature(Arena arena, PveExtension pveExtension) {
        this.arena = arena;
        this.pveExtension = pveExtension;
    }

    public void loadMobs() {
        var random = ThreadLocalRandom.current();
        var stage = arena.getFeature(StageFeature.class).getStage();
        int initialMobCount = Math.min((int) (stage * 1.5), 200);

        List<ArenaMob> initialMobs = new ArrayList<>();
        while (initialMobs.size() < initialMobCount) {
            var generator = MOB_GENERATORS.get(random.nextInt(MOB_GENERATORS.size()));
            generator.generate(arena, initialMobs.size()).ifPresent(initialMobs::add);
        }
        maxMobCount.set(initialMobs.size());

        spawningMobs.addAll(initialMobs);
    }

    public void spawnMobs() {
        var instance = arena.getFeature(InstanceFeature.class).getInstance();
        for (int i = 0; i < pveExtension.getMainConfig().getMobPerSpawn(); i++) {
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
        var timerFeature = arena.getFeature(TimerFeature.class);
        if (timerFeature.getDuration() > 0) return;
        spawnMobs();
        timerFeature.setDuration(pveExtension.getMainConfig().getSpawnDelay());
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

    public int getMaxMobCount() {
        return maxMobCount.get();
    }

    @Override
    public void clear() {
        clearMobs();
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
            return controller((mobArena, count) -> controller.test(count));
        }

        public MobGenerator condition(Predicate<Arena> condition) {
            return controller((mobArena, count) -> condition.test(mobArena));
        }

        public MobGenerator chance(double chance) {
            return condition(mobArena -> Math.random() < chance);
        }

        public MobGenerator preferStage(IntPredicate stagePredicate) {
            return condition(mobArena -> stagePredicate.test(mobArena.getFeature(StageFeature.class).getStage()));
        }

        public MobGenerator minStage(int minStage) {
            return preferStage(stage -> stage >= minStage);
        }

        public MobGenerator preferPlayers(IntPredicate playerPredicate) {
            return condition(mobArena -> playerPredicate.test(mobArena.getFeature(JoinFeature.class).getPlayerCount()));
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
            return preference((mobArena, count) -> preference.applyAsDouble(mobArena), weight);
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
