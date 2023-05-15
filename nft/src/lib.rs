//! # Non Fungible Token
//! The module provides implementations for non-fungible-token.
//!
//! - [`Config`](./trait.Config.html)
//! - [`Call`](./enum.Call.html)
//! - [`Module`](./struct.Module.html)
//!
//! ## Overview
//!
//! This module provides basic functions to create and manager
//! NFT(non fungible token) such as `create_class`, `transfer`, `mint`, `burn`.

//! ### Module Functions
//!
//! - `create_class` - Create NFT(non fungible token) class
//! - `transfer` - Transfer NFT(non fungible token) to another account.
//! - `mint` - Mint NFT(non fungible token)
//! - `burn` - Burn NFT(non fungible token)
//! - `destroy_class` - Destroy NFT(non fungible token) class
//! - `transfer_stackable_nft` - Transfer stackable NFT(non fungible token) balance to another account
//! - `mint_stackable_nft` - Mint stackable NFT(non fungible token)

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(clippy::unused_unit)]

use codec::{Decode, Encode, MaxEncodedLen};
use frame_support::{
    BoundedVec,
    ensure,
    pallet_prelude::*,
    Parameter, traits::{Currency, Get, ReservableCurrency},
};
pub use module::*;
use scale_info::TypeInfo;
use sp_runtime::{
    ArithmeticError,
    DispatchError, DispatchResult, RuntimeDebug, traits::{AtLeast32BitUnsigned, CheckedAdd, CheckedSub, MaybeSerializeDeserialize, Member, One, Saturating, Zero},
};
use sp_std::vec::Vec;

mod mock;
mod tests;

/// Class info
#[derive(Encode, Decode, Clone, Eq, PartialEq, MaxEncodedLen, RuntimeDebug, TypeInfo)]
pub struct ClassInfo<TokenId, AccountId, Data, ClassMetadataOf> {
    /// Class metadata
    pub metadata: ClassMetadataOf,
    /// Total issuance for the class
    pub total_issuance: TokenId,
    /// Class owner
    pub owner: AccountId,
    /// Class Properties
    pub data: Data,
}

/// Token info
#[derive(Encode, Decode, Clone, Eq, PartialEq, MaxEncodedLen, RuntimeDebug, TypeInfo)]
pub struct TokenInfo<AccountId, Data, TokenMetadataOf> {
    /// Token metadata
    pub metadata: TokenMetadataOf,
    /// Token owner
    pub owner: AccountId,
    /// Token Properties
    pub data: Data,
}

#[frame_support::pallet]
pub mod module {
    use super::*;

    #[pallet::config]
    pub trait Config: frame_system::Config {
        /// The class ID type
        type ClassId: Parameter + Member + AtLeast32BitUnsigned + Default + Copy + MaxEncodedLen;
        /// Currency type for reserve/unreserve balance
        type Currency: Currency<Self::AccountId> + ReservableCurrency<Self::AccountId>;
        /// The token ID type
        type TokenId: Parameter + Member + AtLeast32BitUnsigned + Default + Copy + MaxEncodedLen;
        /// The class properties type
        type ClassData: Parameter + Member + MaybeSerializeDeserialize;
        /// The token properties type
        type TokenData: Parameter + Member + MaybeSerializeDeserialize;
        /// The maximum size of a class's metadata
        type MaxClassMetadata: Get<u32>;
        /// The maximum size of a token's metadata
        type MaxTokenMetadata: Get<u32>;
    }

    pub type ClassMetadataOf<T> = BoundedVec<u8, <T as Config>::MaxClassMetadata>;
    pub type TokenMetadataOf<T> = BoundedVec<u8, <T as Config>::MaxTokenMetadata>;
    pub type ClassInfoOf<T> = ClassInfo<
        <T as Config>::TokenId,
        <T as frame_system::Config>::AccountId,
        <T as Config>::ClassData,
        ClassMetadataOf<T>,
    >;
    pub type TokenInfoOf<T> =
    TokenInfo<<T as frame_system::Config>::AccountId, <T as Config>::TokenData, TokenMetadataOf<T>>;

    pub type GenesisTokenData<T> = (
        <T as frame_system::Config>::AccountId, // Token owner
        Vec<u8>,                                // Token metadata
        <T as Config>::TokenData,
    );
    pub type GenesisTokens<T> = (
        <T as frame_system::Config>::AccountId, // Token class owner
        Vec<u8>,                                // Token class metadata
        <T as Config>::ClassData,
        Vec<GenesisTokenData<T>>, // Vector of tokens belonging to this class
    );
    pub type BalanceOf<T> = <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

    /// Error for non-fungible-token module.
    #[pallet::error]
    pub enum Error<T> {
        /// No available class ID
        NoAvailableClassId,
        /// No available token ID
        NoAvailableTokenId,
        /// Token(ClassId, TokenId) not found
        TokenNotFound,
        /// Class not found
        ClassNotFound,
        /// The operator is not the owner of the token and has no permission
        NoPermission,
        /// Can not destroy class
        /// Total issuance is not 0
        CannotDestroyClass,
        /// Failed because the Maximum amount of metadata was exceeded
        MaxMetadataExceeded,
        /// Invalid stackable NFT transfer (stored value is equal to zero)
        InvalidStackableNftTransfer,
        /// Invalid stackable NFT balance
        InvalidStackableNftAmount,
        /// The stackable collection already exists
        StackableCollectionAlreadyExists,
        /// This collection is not autoincrement id
        TokenIdRequired,
        /// Token already exists
        TokenAlreadyExist
    }

    /// Next available class ID.
    #[pallet::storage]
    #[pallet::getter(fn next_class_id)]
    pub type NextClassId<T: Config> = StorageValue<_, T::ClassId, ValueQuery>;

    /// Next available token ID.
    #[pallet::storage]
    #[pallet::getter(fn next_token_id)]
    pub type NextTokenId<T: Config> = StorageMap<_, Twox64Concat, T::ClassId, T::TokenId, ValueQuery>;

    /// Store class info.
    ///
    /// Returns `None` if class info not set or removed.
    #[pallet::storage]
    #[pallet::getter(fn classes)]
    pub type Classes<T: Config> = StorageMap<_, Twox64Concat, T::ClassId, ClassInfoOf<T>>;

    /// Store token info.
    ///
    /// Returns `None` if token info not set or removed.
    #[pallet::storage]
    #[pallet::getter(fn tokens)]
    pub type Tokens<T: Config> =
    StorageDoubleMap<_, Twox64Concat, T::ClassId, Twox64Concat, T::TokenId, TokenInfoOf<T>>;

    /// Token existence check by owner and class ID.
    #[pallet::storage]
    #[pallet::getter(fn tokens_by_owner)]
    pub type TokensByOwner<T: Config> = StorageNMap<
        _,
        (
            NMapKey<Blake2_128Concat, T::AccountId>, // owner
            NMapKey<Blake2_128Concat, T::ClassId>,
            NMapKey<Blake2_128Concat, T::TokenId>,
        ),
        (),
        ValueQuery,
    >;

    #[pallet::storage]
    #[pallet::getter(fn get_stackable_collection)]
    /// Index stackable collections by (class ID, token ID)
    pub(super) type StackableCollection<T: Config> =
    StorageMap<_, Blake2_128Concat, (T::ClassId, T::TokenId), (), OptionQuery>;

    #[pallet::storage]
    #[pallet::getter(fn get_stackable_collections_balances)]
    /// Index stackable collections balances
    pub(super) type StackableCollectionsBalances<T: Config> = StorageNMap<
        _,
        (
            NMapKey<Blake2_128Concat, T::ClassId>,
            NMapKey<Blake2_128Concat, T::TokenId>,
            NMapKey<Blake2_128Concat, T::AccountId>,
        ),
        BalanceOf<T>,
        ValueQuery,
    >;

    #[pallet::genesis_config]
    pub struct GenesisConfig<T: Config> {
        pub tokens: Vec<GenesisTokens<T>>,
    }

    #[cfg(feature = "std")]
    impl<T: Config> Default for GenesisConfig<T> {
        fn default() -> Self {
            GenesisConfig { tokens: vec![] }
        }
    }

    #[pallet::genesis_build]
    impl<T: Config> GenesisBuild<T> for GenesisConfig<T> {
        fn build(&self) {
            self.tokens.iter().for_each(|token_class| {
                let class_id = Pallet::<T>::create_class(&token_class.0, token_class.1.to_vec(), token_class.2.clone())
                    .expect("Create class cannot fail while building genesis");
                for (account_id, token_metadata, token_data) in &token_class.3 {
                    Pallet::<T>::mint(account_id, class_id, token_metadata.to_vec(), token_data.clone())
                        .expect("Token mint cannot fail during genesis");
                }
            })
        }
    }

    #[pallet::pallet]
    #[pallet::generate_store(pub (super) trait Store)]
    #[pallet::without_storage_info]
    pub struct Pallet<T>(_);

    #[pallet::hooks]
    impl<T: Config> Hooks<T::BlockNumber> for Pallet<T> {}

    #[pallet::call]
    impl<T: Config> Pallet<T> {}
}

impl<T: Config> Pallet<T> {
    /// Create NFT(non fungible token) class
    pub fn create_class(
        owner: &T::AccountId,
        metadata: Vec<u8>,
        data: T::ClassData,
    ) -> Result<T::ClassId, DispatchError> {
        let bounded_metadata: BoundedVec<u8, T::MaxClassMetadata> =
            metadata.try_into().map_err(|_| Error::<T>::MaxMetadataExceeded)?;

        let class_id = NextClassId::<T>::try_mutate(|id| -> Result<T::ClassId, DispatchError> {
            let current_id = *id;
            *id = id.checked_add(&One::one()).ok_or(Error::<T>::NoAvailableClassId)?;
            Ok(current_id)
        })?;

        let info = ClassInfo {
            metadata: bounded_metadata,
            total_issuance: Default::default(),
            owner: owner.clone(),
            data,
        };
        Classes::<T>::insert(class_id, info);

        Ok(class_id)
    }

    /// Transfer NFT(non fungible token) from `from` account to `to` account
    pub fn transfer(from: &T::AccountId, to: &T::AccountId, token: (T::ClassId, T::TokenId)) -> DispatchResult {
        Tokens::<T>::try_mutate(token.0, token.1, |token_info| -> DispatchResult {
            let mut info = token_info.as_mut().ok_or(Error::<T>::TokenNotFound)?;
            ensure!(info.owner == *from, Error::<T>::NoPermission);
            if from == to {
                // no change needed
                return Ok(());
            }

            info.owner = to.clone();

            TokensByOwner::<T>::remove((from, token.0, token.1));
            TokensByOwner::<T>::insert((to, token.0, token.1), ());

            Ok(())
        })
    }

    /// Mint NFT(non fungible token) to `owner`
    pub fn mint(
        owner: &T::AccountId,
        class_id: T::ClassId,
        metadata: Vec<u8>,
        data: T::TokenData,
    ) -> Result<T::TokenId, DispatchError> {
        NextTokenId::<T>::try_mutate(class_id, |id| -> Result<T::TokenId, DispatchError> {
            let bounded_metadata: BoundedVec<u8, T::MaxTokenMetadata> =
                metadata.try_into().map_err(|_| Error::<T>::MaxMetadataExceeded)?;

            let token_id = *id;
            let next_token_id = token_id.checked_add(&One::one()).ok_or(Error::<T>::NoAvailableTokenId)?;
            // Ensure token is not exists
            ensure!(!Tokens::<T>::contains_key(class_id, next_token_id), Error::<T>::TokenAlreadyExist);

            *id = id.checked_add(&One::one()).ok_or(Error::<T>::NoAvailableTokenId)?;

            Classes::<T>::try_mutate(class_id, |class_info| -> DispatchResult {
                let info = class_info.as_mut().ok_or(Error::<T>::ClassNotFound)?;
                info.total_issuance = info
                    .total_issuance
                    .checked_add(&One::one())
                    .ok_or(ArithmeticError::Overflow)?;
                Ok(())
            })?;

            let token_info = TokenInfo {
                metadata: bounded_metadata,
                owner: owner.clone(),
                data,
            };
            Tokens::<T>::insert(class_id, token_id, token_info);
            TokensByOwner::<T>::insert((owner, class_id, token_id), ());

            Ok(token_id)
        })
    }

    /// Mint NFT(non fungible token) to `owner`
    pub fn mint_with_token_id(
        owner: &T::AccountId,
        class_id: T::ClassId,
        token_id: T::TokenId,
        metadata: Vec<u8>,
        data: T::TokenData,
    ) -> Result<T::TokenId, DispatchError> {
        // Ensure autoincrement NextTokenId does not exists
        ensure!(!NextTokenId::<T>::contains_key(class_id), Error::<T>::TokenIdRequired);

        // Ensure token id doesn't exists
        ensure!(!Tokens::<T>::contains_key(class_id, token_id), Error::<T>::NoAvailableTokenId);

        let bounded_metadata: BoundedVec<u8, T::MaxTokenMetadata> =
            metadata.try_into().map_err(|_| Error::<T>::MaxMetadataExceeded)?;

        Classes::<T>::try_mutate(class_id, |class_info| -> DispatchResult {
            let info = class_info.as_mut().ok_or(Error::<T>::ClassNotFound)?;
            info.total_issuance = info
                .total_issuance
                .checked_add(&One::one())
                .ok_or(ArithmeticError::Overflow)?;
            Ok(())
        })?;

        let token_info = TokenInfo {
            metadata: bounded_metadata,
            owner: owner.clone(),
            data,
        };
        Tokens::<T>::insert(class_id, token_id, token_info);
        TokensByOwner::<T>::insert((owner, class_id, token_id), ());

        Ok(token_id)
    }

    /// Burn NFT(non fungible token) from `owner`
    pub fn burn(owner: &T::AccountId, token: (T::ClassId, T::TokenId)) -> DispatchResult {
        Tokens::<T>::try_mutate_exists(token.0, token.1, |token_info| -> DispatchResult {
            let t = token_info.take().ok_or(Error::<T>::TokenNotFound)?;
            ensure!(t.owner == *owner, Error::<T>::NoPermission);

            Classes::<T>::try_mutate(token.0, |class_info| -> DispatchResult {
                let info = class_info.as_mut().ok_or(Error::<T>::ClassNotFound)?;
                info.total_issuance = info
                    .total_issuance
                    .checked_sub(&One::one())
                    .ok_or(ArithmeticError::Overflow)?;
                Ok(())
            })?;

            TokensByOwner::<T>::remove((owner, token.0, token.1));

            Ok(())
        })
    }

    /// Destroy NFT(non fungible token) class
    pub fn destroy_class(owner: &T::AccountId, class_id: T::ClassId) -> DispatchResult {
        Classes::<T>::try_mutate_exists(class_id, |class_info| -> DispatchResult {
            let info = class_info.take().ok_or(Error::<T>::ClassNotFound)?;
            ensure!(info.owner == *owner, Error::<T>::NoPermission);
            ensure!(info.total_issuance == Zero::zero(), Error::<T>::CannotDestroyClass);

            NextTokenId::<T>::remove(class_id);

            Ok(())
        })
    }

    /// Checks if account owns NFT
    pub fn is_owner(account: &T::AccountId, token: (T::ClassId, T::TokenId)) -> bool {
        TokensByOwner::<T>::contains_key((account, token.0, token.1))
    }

    /// Mint stackable NFT
    pub fn mint_stackable_nft(
        owner: &T::AccountId,
        class_id: T::ClassId,
        metadata: Vec<u8>,
        data: T::TokenData,
        amount: BalanceOf<T>,
    ) -> Result<(T::TokenId, BalanceOf<T>), DispatchError> {
        ensure!(amount > Zero::zero(), Error::<T>::InvalidStackableNftAmount);

        NextTokenId::<T>::try_mutate(class_id, |id| -> Result<(T::TokenId, BalanceOf<T>), DispatchError> {
            let bounded_metadata: BoundedVec<u8, T::MaxTokenMetadata> =
                metadata.try_into().map_err(|_| Error::<T>::MaxMetadataExceeded)?;

            let token_id = *id;
            *id = id.checked_add(&One::one()).ok_or(Error::<T>::NoAvailableTokenId)?;

            Classes::<T>::try_mutate(class_id, |class_info| -> DispatchResult {
                let info = class_info.as_mut().ok_or(Error::<T>::ClassNotFound)?;
                info.total_issuance = info
                    .total_issuance
                    .checked_add(&One::one())
                    .ok_or(ArithmeticError::Overflow)?;
                Ok(())
            })?;

            let token_info = TokenInfo {
                metadata: bounded_metadata,
                owner: owner.clone(),
                data,
            };
            Tokens::<T>::insert(class_id, token_id, token_info);
            TokensByOwner::<T>::insert((owner, class_id, token_id), ());

            // Not likely to happen but ensure that the stackable collection balance is not
            // already set
            ensure!(
				Self::get_stackable_collections_balances((class_id, token_id, owner.clone())) == Zero::zero(),
				Error::<T>::StackableCollectionAlreadyExists
			);

            StackableCollectionsBalances::<T>::insert((class_id, token_id, owner.clone()), amount);
            StackableCollection::<T>::insert((class_id, token_id), ());
            Ok((token_id, amount))
        })
    }

    /// Transfer stackable NFT
    pub fn transfer_stackable_nft(
        from: &T::AccountId,
        to: &T::AccountId,
        asset_id: (T::ClassId, T::TokenId),
        amount: BalanceOf<T>,
    ) -> DispatchResultWithPostInfo {
        StackableCollectionsBalances::<T>::try_mutate(
            (asset_id.0, asset_id.1, from.clone()),
            |sender_balance| -> DispatchResultWithPostInfo {
                StackableCollectionsBalances::<T>::try_mutate(
                    (asset_id.0, asset_id.1, to.clone()),
                    |receiver_balance| -> DispatchResultWithPostInfo {
                        ensure!(
							amount > Zero::zero()
								&& Self::get_stackable_collections_balances((asset_id.0, asset_id.1, from.clone()))
									>= amount,
							Error::<T>::InvalidStackableNftTransfer
						);

                        *receiver_balance = receiver_balance.checked_add(&amount).ok_or(ArithmeticError::Overflow)?;
                        *sender_balance = sender_balance.checked_sub(&amount).ok_or(ArithmeticError::Overflow)?;

                        Ok(().into())
                    },
                )
            },
        )
    }

    /// Checks if token is stackable
    pub fn is_stackable(token: (T::ClassId, T::TokenId)) -> Result<bool, DispatchError> {
        Ok(Self::get_stackable_collection(token).is_some())
    }
}
